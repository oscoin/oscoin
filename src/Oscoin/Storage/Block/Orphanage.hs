{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Storage.Block.Orphanage
    ( Orphanage -- opaque
    , emptyOrphanage
    , getOrphans
    , size
    , member
    , candidateAvailable
    , insertOrphan
    , toBlocksOldestFirst
    , selectBestChain
    , fromChainSuffix

    -- * Pruning the orphanage
    , pruneOrphanageShallow
    , pruneOrphanageDeep

    -- * Internal use only
    , sizeAt
    , candidates
    , tips
    , ChainCandidate(..)
    ) where

import           Oscoin.Prelude
import qualified Prelude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (><), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Oscoin.Crypto.Blockchain hiding (parentHash, (|>))
import qualified Oscoin.Crypto.Blockchain as Block
import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Time.Chrono

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type Index = Int
type ChainRoot c = BlockHash c
type Candidates c s = Vector (ChainCandidate c s)

data Orphanage c tx s = Orphanage
    { orphans    :: Map (BlockHash c) (Block c tx (Sealed c s))
    -- ^ The /actual/ blocks, indexed by their block hash and in no particular
    -- order.
    , candidates :: Map (ChainRoot c) (Candidates c s)
    -- ^ The chain candidates, indexed by the /parent hash/ of the /root/ of
    -- the chain (i.e. the oldest block and the first in the sequence).
    , tips       :: Map (BlockHash c) (ChainRoot c, Index)
    -- ^ /All/ the tips of /all/ the chain candidates in the system. This
    -- allows fast lookup when we need to merge two chains together. The
    -- value indexed by this map is the 'BlockHash' to be used to lookup the
    -- candidates and the 'Index' of the particular candidate where the tip
    -- has to be found.
    , scoreBlock :: Block c tx (Sealed c s) -> Score
    -- ^ A function to assign a 'Score' to a block.
    }

-- Bogus instance created only to please the test code.
instance Show (Orphanage c tx s) where
    show _ = "<orphanage>"

-- | A 'ChainCandidate' is a sequence of blocks and a partially-accumulated
-- 'Score', used to compute the best chain in 'selectBestChain'. The chain
-- is stored \"oldest first\", i.e. from the oldest to the newest block.
data ChainCandidate c s = ChainCandidate
    { candidateChain    :: OldestFirst Seq (BlockHash c)
    -- ^ The actual candidate chain, stored as a sequence of hashes.
    , candidateScore    :: Score
    -- ^ The total 'Score' for this candidate
    , candidateRootHash :: BlockHash c
    -- ^ The hash of the root block for this candidate, i.e. the 'blockPrevHash'
    -- of the oldest block of this candidate.
    }

deriving instance (Show (Hash c), Show s) => Show (ChainCandidate c s)
deriving instance (Eq (Hash c), Eq s) => Eq (ChainCandidate c s)

instance (Ord (Hash c), Eq s) => Ord (ChainCandidate c s) where
    compare chain1 chain2 =
        let compareScore  = candidateScore chain1 `compare` candidateScore chain2
            compareLength = Seq.length (toOldestFirst $ candidateChain chain1) `compare`
                            Seq.length (toOldestFirst $ candidateChain chain2)
            compareTipHash = lookupChainTip chain1 `compare` lookupChainTip chain2
        -- If we score a draw, pick the longest.  If we draw a score /again/,
        -- compare the hash of the tip header.
        in mconcat [compareScore, compareLength, compareTipHash]

{------------------------------------------------------------------------------
  Operations on chain candidates
------------------------------------------------------------------------------}

-- | /O(1)/ Creates a new 'ChainCandidate' from the input 'BlockWithScore'.
chainCandidate
    :: (Block c tx (Sealed c s) -> Score)
    -> Block c tx (Sealed c s)
    -> ChainCandidate c s
chainCandidate scoreBlock b = ChainCandidate
    { candidateChain = OldestFirst $ Seq.singleton (blockHash b)
    , candidateScore = scoreBlock b
    , candidateRootHash = blockPrevHash . blockHeader $ b
    }

-- | /O(n)/ Builds a 'ChainCandidate' out of a list of blocks. Very useful to turn
-- a chain suffix on the main chain into a 'ChainCandidate' for comparison,
-- to determine which of the two won.
fromChainSuffix :: (Block c tx (Sealed c s) -> Score)
                -> OldestFirst NonEmpty (Block c tx (Sealed c s))
                -> ChainCandidate c s
fromChainSuffix scoreBlock (toOldestFirst -> blks) = ChainCandidate
    { candidateChain = OldestFirst
                     . Seq.fromList
                     . NonEmpty.toList
                     . map blockHash
                     $ blks
    , candidateScore = sum (map scoreBlock blks)
    , candidateRootHash = blockPrevHash
                        . blockHeader
                        . NonEmpty.head
                        $ blks
    }

-- | /O(n * log(n))/. Returns a list of blocks out of this 'ChainCandidate',
-- ordered with the oldest first.
toBlocksOldestFirst
    :: Ord (BlockHash c)
    => Orphanage c tx s
    -> ChainCandidate c s
    -> OldestFirst NonEmpty (Block c tx (Sealed c s))
toBlocksOldestFirst Orphanage{orphans} ChainCandidate{candidateChain} =
    OldestFirst $ fromMaybe (panic "toBlocksOldestFirst: the candidate chain was empty.")
                $ traverse (`Map.lookup` orphans) (NonEmpty.fromList . toList $ candidateChain)

-- | /O(1)/. Looks up the tip (i.e. the newest) block for this 'ChainCandidate'.
lookupChainTip :: ChainCandidate c s -> Maybe (BlockHash c)
lookupChainTip (toOldestFirst . candidateChain -> chain) =
    case Seq.viewr chain of
        EmptyR -> Nothing
        _ :> b -> Just b

-- | /O(1)/. Looks up the root (i.e. the oldest) block for this 'ChainCandidate'.
lookupChainRoot :: ChainCandidate c s -> Maybe (BlockHash c)
lookupChainRoot (toOldestFirst . candidateChain -> chain) =
    case Seq.viewl chain of
        EmptyL -> Nothing
        b :< _ -> Just b

-- | /O(1)/ Prepend the block /in front/ of this 'ChainCandidate'. The tip stays
-- the same.
consBlock :: (Block c tx (Sealed c s) -> Score)
          -> Block c tx (Sealed c s)
          -> ChainCandidate c s
          -> ChainCandidate c s
consBlock scoreBlock b cc =
    cc { candidateChain = mapOF (blockHash b <|) (candidateChain cc)
       , candidateScore = scoreBlock b + candidateScore cc
       , candidateRootHash = blockPrevHash . blockHeader $ b
       }

-- | \( O(\log(\min(n_1,n_2))) \) Concatenate two candidates together, where
-- the second chain /extends/ the first one.
consCandidates :: ChainCandidate c s -> ChainCandidate c s -> ChainCandidate c s
consCandidates chain1 chain2 =
    ChainCandidate
        { candidateChain = mapOF ((><) . toOldestFirst . candidateChain $ chain1)
                                 (candidateChain chain2)
        , candidateScore = candidateScore chain1 + candidateScore chain2
        , candidateRootHash = candidateRootHash chain1
        }

-- | O(1) Append the block /at the back/ of this 'ChainCandidate', which results
-- in a new tip.
snocBlock :: (Block c tx (Sealed c s) -> Score)
          -> Block c tx (Sealed c s)
          -> ChainCandidate c s
          -> ChainCandidate c s
snocBlock scoreBlock b cc =
    cc { candidateChain = mapOF (|> blockHash b) (candidateChain cc)
       , candidateScore = scoreBlock b + candidateScore cc
       }

{------------------------------------------------------------------------------
  Operations on the 'Orphanage'.
------------------------------------------------------------------------------}

-- | Returns the size (i.e. the number of chain candidates) for a given
-- 'BlockHash' in the orphanage.
-- Internal use only.
sizeAt
    :: Ord (Hash c)
    => Hash c
    -> Orphanage c tx s
    -> Int
sizeAt rootHash (Orphanage _ chains _ _) =
    case Map.lookup rootHash chains of
      Nothing -> 0
      Just cs -> length cs


-- | /O(1)/ Creates a new, empty 'Orphanage'.
emptyOrphanage
    :: Ord (Hash c)
    => (Block c tx (Sealed c s) -> Score)
    -> Orphanage c tx s
emptyOrphanage = Orphanage mempty mempty mempty

getOrphans :: Orphanage c tx s -> Set (BlockHash c)
getOrphans = Map.keysSet . orphans

member :: Ord (BlockHash c) => Orphanage c tx s -> BlockHash c -> Bool
member o h = Set.member h . Map.keysSet . orphans $ o

-- | /O(1)/ The size of the 'Orphanage', i.e. how many /unique/ orphans are
-- currently stored.
size :: Orphanage c tx s -> Int
size = Map.size . orphans

-- | /O(log n)/ Returns 'True' if the input candidate matches an available chain in
-- the orphanage.
candidateAvailable
    :: Ord (Hash c)
    => Hash c
    -> Orphanage c tx s
    -> Bool
candidateAvailable rootHash Orphanage{candidates} =
    isJust (Map.lookup rootHash candidates)

-- | /O(log(n))/ Picks the best candidate out of the candidate set at the given 'BlockHash'.
selectBestCandidate
    :: (Ord (Hash c), Eq s)
    => BlockHash c
    -> Orphanage c tx s
    -> Maybe (ChainCandidate c s)
selectBestCandidate blockOnMainChain o =
    case sortOn Down . Vector.toList <$> Map.lookup blockOnMainChain (candidates o) of
        Nothing    -> Nothing
        Just []    -> Nothing
        Just (c:_) -> Just c

-- | /O(n * log(n))/ Given a list of block hashes on the main chain, selects the best-scoring
-- 'ChainCandidate' which would extend the chain from that given block.
selectBestChain
    :: (Ord (Hash c), Eq s)
    => [BlockHash c]
    -> Orphanage c tx s
    -> Maybe (BlockHash c, ChainCandidate c s)
selectBestChain blocksOnMainChain o =
    case sortOn (Down . snd) . catMaybes $
         [(h,) <$> selectBestCandidate h o | h <- blocksOnMainChain] of
        []  -> Nothing
        c:_ -> Just c

-- | /O(m)/ Evicts the candidates branching off the input block hash from the 'Orphanage'.
bulkDelete
    :: forall c tx s. Ord (BlockHash c)
    => BlockHash c
    -> Orphanage c tx s
    -> Orphanage c tx s
bulkDelete bHash o =
    o { candidates = candidates' (candidates o)
      , tips = tips' (tips o)
      }
  where
      candidates' :: Map (ChainRoot c) (Candidates c s)
                  -> Map (ChainRoot c) (Candidates c s)
      candidates' = Map.delete bHash

      tips' :: Map (BlockHash c) (ChainRoot c, Index)
            -> Map (BlockHash c) (ChainRoot c, Index)
      tips' oldTips =
          let updateFn mp staleChain =
                  case lookupChainTip staleChain of
                    Nothing -> mp
                    Just t  -> Map.delete t mp
          in case Map.lookup bHash (candidates o) of
              Nothing          -> oldTips
              Just staleChains -> foldl' updateFn oldTips staleChains

-- | Extends the orphanage with new candidates associated to a 'ChainRoot'.
-- It takes into account the possibility that an existing set of candidates
-- might be present at that @ptr@, so this function won't replace them.
bulkAdjust
    :: forall c tx s. Ord (BlockHash c)
    => ChainRoot c
    -> Candidates c s
    -> Orphanage c tx s
    -> Orphanage c tx s
bulkAdjust ptr newChains o =
    o { candidates = candidates'
      , tips = tips'
      }
  where
      candidates' :: Map (ChainRoot c) (Candidates c s)
      candidates' = Map.alter (\maybeChains -> Just $ fromMaybe mempty maybeChains <> newChains) ptr (candidates o)

      tips' :: Map (BlockHash c) (ChainRoot c, Index)
      tips' =
          -- Where do we start zipping is a function of whether or not
          -- we had existing candidates at @ptr@.
          let start = maybe 0 length (Map.lookup ptr (candidates o))
              newCandidates = Vector.zip newChains (Vector.fromList [start .. ])
              updateFn mp (newChain, ix) =
                  case lookupChainTip newChain of
                    Nothing -> mp
                    Just t  -> Map.insert t (ptr, ix) mp
          in foldl' updateFn (tips o) newCandidates

-- | Stores the orphan block into the internal storage.
storeOrphan
    :: Ord (BlockHash c)
    => Block c tx (Sealed c s)
    -> Orphanage c tx s
    -> Orphanage c tx s
storeOrphan b o = o { orphans = Map.insert (blockHash b) b (orphans o) }

{------------------------------------------------------------------------------

                                Pruning chains

 If a suitable 'ChainCandidate' has been found, the winning chain needs to be
 removed from the 'Orphanage'. We offer two different functions, depending from
 the context: 'pruneOrphanageShallow' removes only the winning chain, but it
 doesn't touch any chain branching from any of the blocks of the chain being
 pruned. If that's not the desired behaviour, you might want to consider using
 'pruneOrphanageDeep'. In pictures:


          ORIGINAL                   SHALLOW                         DEEP

                 |                         |                            |
                 |                         |                            |
                 |                         |                            |
    Winning      |                         |                            |
    Candidate    |                         |                            |
            +-+  |                         |                            |
            | | /\                         \                            \
            | |-  -\                        -\                           -\
            |<|     >                         >                            >
            |||
            |||
            |v|
            |-|                       /-
          /-|||                     /-
        /-  |||                   /-
      <-    |||                 <-
            |v|                         \
            | |\                         \
            | | v                         v
            | |
            +-+

------------------------------------------------------------------------------}

-- | Like 'pruneOrphanageShallow', but also prunes any other candidate chain
-- branching off from /any/ block of the chain being pruned.
pruneOrphanageDeep
    :: forall c tx s.
       ( Ord (BlockHash c)
       , Eq s
       )
    => ChainCandidate c s
    -> Orphanage c tx s
    -> Orphanage c tx s
pruneOrphanageDeep bestFork o = prune o [bestFork]
  where
      prune :: Orphanage c tx s
            -> [ChainCandidate c s]
            -> Orphanage c tx s
      prune orph [] = orph
      prune orph (c:cs) =
          let branchingForks = map Vector.toList $ catMaybes $
                  map (`Map.lookup` candidates orph)
                      (toList $ candidateChain c)
          in prune (pruneOrphanageShallow c orph)
                   (cs <> mconcat branchingForks)

-- | Given a candidate which won, delete the orphans from the store and
-- the chain from the candidates.
pruneOrphanageShallow
    :: forall c tx s.
       ( Ord (BlockHash c)
       , Eq s
       )
    => ChainCandidate c s
    -> Orphanage c tx s
    -> Orphanage c tx s
pruneOrphanageShallow inputCandidate@ChainCandidate{candidateChain, candidateRootHash} o =
    let (candidates', canPruneOrphans) = pruneCandidate (candidates o)
        orphans' = if canPruneOrphans
                      then foldl' (flip Map.delete) (orphans o) (toList candidateChain)
                      else orphans o
    in o { orphans = orphans'
         , candidates = candidates'
         , tips = tips' (tips o)
         }
  where
      -- Prunes the input 'ChainCandidate'. If this was the only candidate
      -- for the root hash, also prune the orphan blocks.
      -- N.B (adn) Is this sufficient or we need full-flow reference counting?
      pruneCandidate ::  Map (ChainRoot c) (Candidates c s)
                     -> (Map (ChainRoot c) (Candidates c s), Bool)
      pruneCandidate old =
          case map removeCandidate (Map.lookup candidateRootHash old) of
            Nothing -> (old, False)
            Just xs | Vector.null xs -> (Map.delete candidateRootHash old, True)
            Just xs -> (Map.insert candidateRootHash xs old, False)

      removeCandidate = Vector.filter (inputCandidate /=)

      tips' :: Map (BlockHash c) (ChainRoot c, Index)
            -> Map (BlockHash c) (ChainRoot c, Index)
      tips' oldTips =
          case lookupChainTip inputCandidate of
              Nothing -> oldTips
              Just t  -> Map.delete t oldTips

-- | Adds a new 'Block' to the 'Orphanage'. Three scenarios are possible:
-- 1. This is a \"singleton\" 'Block', i.e. it doesn't extend any of the
--    existing chains. In this case, we simply insert it as a potential
--    candidate alongside the others.
-- 2. This block is the /parent/ of an existing set of candidates. In this
--    case, we iterate over all the candidates and add the block at the left
--    end of each sequence, as well as replacing the old, stale set with a
--    new one, which now points to the parent hash of the incoming block.
-- 3. This block extends one of the chain candidates at the end (i.e. this is
--    a continuation of one of the chains. This is the trickiest scenario to
--    implement efficiently.
--
insertOrphan
    :: forall c tx s. Ord (BlockHash c)
    => Block c tx (Sealed c s)
    -> Orphanage c tx s
    -> Orphanage c tx s
insertOrphan b (storeOrphan b -> o) = withFrozenCallStack $
  maybe o tryLinkChains (extendInFront <|> extendExisting)

  where
    bHash             = blockHash b
    parentHash        = Block.parentHash $ b
    currentCandidates = candidates o

    -- Check if this block is the parent of any existing chain, and if it is,
    -- this means we have to add it at the /front/ of
    -- of all the chain candidates that branch off from this block,
    -- as well as updating the outermost map.
    -- N.B. There is one special exception to this: if the input block is
    -- also a /shared/ block between chains, it would be wrong to call
    -- 'bulkDelete'. Rather what we should be doing is to "promote" the
    -- chain to be a candidate of one of the existing ones.
    extendInFront :: Maybe (Orphanage c tx s)
    extendInFront = do
        chains <- Map.lookup bHash currentCandidates
        let chains' = map (consBlock (scoreBlock o) b) chains
        pure $ bulkAdjust parentHash chains' (bulkDelete bHash o)

    extendExisting :: Maybe (Orphanage c tx s)
    extendExisting = tryExtendChain b o <|> addSingleton

    addSingleton :: Maybe (Orphanage c tx s)
    addSingleton = Just $
        case Map.lookup parentHash currentCandidates of
          Just existingCandidates ->
              let updateFn old = old <> Vector.fromList [chainCandidate (scoreBlock o) b]
              in o { candidates = Map.adjust updateFn parentHash (candidates o)
                , tips       =
                    -- For the tips, what we need to do is to insert the new tip
                    -- by passing as 'ChainRoot' the parentHash, and as the index
                    -- the size of the existing candidates (e.g. if we are about to
                    -- insert the 2nd candidate, the size of @existingCandidates@ would
                    -- be 1, which is also the correct index when looking things up with
                    -- List.!!
                    Map.insert (blockHash b) (parentHash, length existingCandidates) (tips o)
                }
          Nothing ->
              -- This is genuinely a \"singleton\" chain, and we simply
              -- store it.
              insertSingletonChain parentHash b o

    -- If in the candidates we have the /parent/ of the block being inserted
    -- /and/ it also appears to be in the tips, we need to fuse two chains
    -- together.
    tryLinkChains :: Orphanage c tx s -> Orphanage c tx s
    tryLinkChains orph = fromMaybe orph $ do
        candidateChains <- Map.lookup parentHash (candidates orph)
        (chainPtr, ix)  <- Map.lookup parentHash (tips orph)
        -- N.B. we need to keep around all the candidates for this @chainPtr@
        -- and call 'replaceAt' on this list, otherwise we would nuke any
        -- existing (but uncorrelated) candidate branching from this @chainPtr@
        -- which doesn't need to be fused.
        allCandidates   <- Map.lookup chainPtr (candidates o)
        let chainToExtend = allCandidates `unsafeIndex` ix
        -- If we find a match, we need to "attach" all the blocks from the
        -- /chainToPrepend/ to the /chainToExtend/.
        let predFn c = ((bHash ==) <$> lookupChainRoot c) == Just True
        (chainToPrepend, ix') <- findCandidate predFn candidateChains
        chainTip <- lookupChainTip chainToPrepend
        let newChain = chainToExtend `consCandidates` chainToPrepend
        -- Finally, we need to remove the stale candidate(s).
        pure $ orph
            { candidates = Map.insert chainPtr (replaceAt ix' newChain allCandidates)
                         . Map.delete parentHash
                         $ candidates orph
            , tips = Map.adjust (const (chainPtr, ix')) chainTip
                   . Map.delete parentHash
                   $ tips orph
            }

-- | A variation over 'Vector.!' that fails with a stacktrace.
unsafeIndex :: HasCallStack => Vector a -> Int -> a
v `unsafeIndex` ix = withFrozenCallStack $
    let msg = Prelude.unwords [ "Orphanage.unsafeIndex: index out of range"
                              , "(" , show ix , "," , show (length v) , ")"
                              ]
    in fromMaybe (Prelude.error msg) (v Vector.!? ix)

findCandidate :: (ChainCandidate c s -> Bool)
              -> Candidates c s
              -> Maybe (ChainCandidate c s, Index)
findCandidate predFn xs =
    let zipped = Vector.zip xs (Vector.fromList [0..])
    in head $ Vector.filter (\(c,_) -> predFn c) zipped

insertSingletonChain :: Ord (BlockHash c)
                     => BlockHash c
                     -> Block c tx (Sealed c s)
                     -> Orphanage c tx s
                     -> Orphanage c tx s
insertSingletonChain parentHash b o =
    o { candidates = Map.insert parentHash (Vector.fromList [chainCandidate (scoreBlock o) b]) (candidates o)
      , tips       = Map.insert (blockHash b) (parentHash, 0) (tips o)
      }

-- | Tries extending an existing 'ChainCandidate' with the input block. It works
-- by looking up the /parent/ of the incoming block in the @tips@ map. If a
-- match is found, it means this block effectively extends one of the chains.
tryExtendChain
    :: Ord (Hash c)
    => Block c tx (Sealed c s)
    -> Orphanage c tx s
    -> Maybe (Orphanage c tx s)
tryExtendChain b o = do
    let oldTip = blockPrevHash . blockHeader $ b
    (parentBlockHash, candidateIndex) <- Map.lookup oldTip (tips o)
    candidateChain <- (`unsafeIndex` candidateIndex) <$> Map.lookup parentBlockHash (candidates o)

    -- We now extend the 'ChainCandidate' with the new block.
    let !chain' = snocBlock (scoreBlock o) b candidateChain
    let candidates' = Map.adjust (replaceAt candidateIndex chain') parentBlockHash
                    $ candidates o
    let tips' = Map.insert (blockHash b) (parentBlockHash, candidateIndex)
              . Map.delete oldTip
              $ tips o
    -- Now the tip changed -- we have to remove the old one from @tips@
    pure $ o { candidates = candidates'
             , tips = tips'
             }

-- | Replaces the content of the list at index 'i' with the new value. If the
-- index is out of range, the initial list is returned.
-- >>> replaceAt 0 (-1) [1,2,3]
-- [-1, 2, 3]
-- >>> replaceAt 1 (-1) [1,2,3]
-- [1, -1, 3]
-- >>> replaceAt 3 (-1) [1,2,3]
-- [1, 2, 3]
replaceAt :: Int -> a -> Vector a -> Vector a
replaceAt ix new v = v Vector.// [(ix, new)]
