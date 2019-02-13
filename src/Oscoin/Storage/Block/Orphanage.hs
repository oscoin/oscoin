module Oscoin.Storage.Block.Orphanage
    ( Orphanage -- opaque
    , emptyOrphanage
    , getOrphans
    , insertOrphan
    , toBlocksOldestFirst
    , selectBestChain
    , fromChainSuffix
    , pruneOrphanage
    -- * Internal use only
    , ChainCandidate(..)
    ) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (><), (|>))
import qualified Data.Sequence as Seq
import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain hiding (parentHash, (|>))
import qualified Oscoin.Crypto.Blockchain as Block

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type Index = Int
type ChainRoot = BlockHash
type Candidates s = [ChainCandidate s]

data Orphanage tx s = Orphanage
    { orphans    :: Map BlockHash (Block tx s)
    -- ^ The /actual/ blocks, indexed by their block hash and in no particular
    -- order.
    , candidates :: Map ChainRoot (Candidates s)
    -- ^ The chain candidates, indexed by the /parent hash/ of the /tail/ of
    -- the chain.
    , tips       :: Map BlockHash (ChainRoot, Index)
    -- ^ /All/ the tips of /all/ the chain candidates in the system. This
    -- allows fast lookup when we need to merge two chains together. The
    -- value indexed by this map is the 'BlockHash' to be used to lookup the
    -- candidates and the 'Index' of the particular candidate where the tip
    -- has to be found.
    , scoreBlock :: Block tx s -> Score
    -- ^ A function to assign a 'Score' to a block.
    }

-- | A 'ChainCandidate' is a sequence of blocks and a partially-accumulated
-- 'Score', used to compute the best chain in 'selectBestChain'. The chain
-- is stored \"oldest first\", i.e. from the oldest to the newest block.
data ChainCandidate s = ChainCandidate
    { candidateChain     :: Seq BlockHash
    -- ^ The actual candidate chain, stored as a sequence of hashes.
    , candidateTipHeader :: BlockHeader s
    -- ^ The 'BlockHeader' of the current tip, to be used (when needed) for
    -- the comparison with other candidates.
    , candidateScore     :: Score
    -- ^ The total 'Score' for this candidate
    } deriving (Eq, Show)

instance Eq s => Ord (ChainCandidate s) where
    compare chain1 chain2 =
        let compareScore  = candidateScore chain1 `compare` candidateScore chain2
            compareLength = Seq.length (candidateChain chain1) `compare`
                            Seq.length (candidateChain chain2)
            compareTipHash = lookupChainTip chain1 `compare` lookupChainTip chain2
        in case compareScore of
          -- If we score a draw, pick the longest.
          EQ -> case compareLength of
                  -- If we draw a score /again/, compare the hash of the
                  -- tip header.
                  EQ -> compareTipHash
                  y  -> y
          x  -> x

{------------------------------------------------------------------------------
  Operations on chain candidates
------------------------------------------------------------------------------}

-- | /O(1)/ Creates a new 'ChainCandidate' from the input 'BlockWithScore'.
chainCandidate :: (Block tx s -> Score) -> Block tx s -> ChainCandidate s
chainCandidate scoreBlock b = ChainCandidate
    { candidateChain = Seq.singleton (blockHash b)
    , candidateTipHeader = blockHeader b
    , candidateScore = scoreBlock b
    }

-- | /O(n)/ Builds a 'ChainCandidate' out of a list of blocks. Very useful to turn
-- a chain suffix on the main chain into a 'ChainCandidate' for comparison,
-- to determine which of the two won.
fromChainSuffix :: (Block tx s -> Score)
                -> NonEmpty (Block tx s)
                -> ChainCandidate s
fromChainSuffix scoreBlock blks = ChainCandidate
    { candidateChain = Seq.fromList . NonEmpty.toList . map blockHash $ blks
    , candidateTipHeader = blockHeader . NonEmpty.head $ blks
    , candidateScore = sum (map scoreBlock blks)
    }

-- | /O(n * log(n))/. Returns a list of blocks out of this 'ChainCandidate',
-- ordered with the oldest first.
toBlocksOldestFirst :: Orphanage tx s -> ChainCandidate s -> [Block tx s]
toBlocksOldestFirst Orphanage{orphans} ChainCandidate{candidateChain} =
    fromMaybe (panic "toBlocksOldestFirst: the candidate chain was empty.") $
        traverse (`Map.lookup` orphans) (toList candidateChain)

-- | /O(1)/. Looks up the tip (i.e. the newest) block for this 'ChainCandidate'.
lookupChainTip :: ChainCandidate s -> Maybe BlockHash
lookupChainTip cc = case Seq.viewr (candidateChain cc) of
                      EmptyR -> Nothing
                      _ :> b -> Just b

-- | /O(1)/. Looks up the root (i.e. the oldest) block for this 'ChainCandidate'.
lookupChainRoot :: ChainCandidate s -> Maybe BlockHash
lookupChainRoot cc = case Seq.viewl (candidateChain cc) of
                      EmptyL -> Nothing
                      b :< _ -> Just b

-- | O(1) Prepend the block /in front/ of this 'ChainCandidate'. The tip stays
-- the same.
consBlock :: forall tx s. (Block tx s -> Score)
          -> Block tx s
          -> ChainCandidate s
          -> ChainCandidate s
consBlock scoreBlock b cc =
    cc { candidateChain = blockHash b <| candidateChain cc
       , candidateScore = scoreBlock b + candidateScore cc
       }

-- | \( O(\log(\min(n_1,n_2))) \) Concatenate two candidates together, where
-- the second chain /extends/ the first one.
consCandidates :: ChainCandidate s -> ChainCandidate s -> ChainCandidate s
consCandidates chain1 chain2 =
    ChainCandidate
        { candidateChain = candidateChain chain1 >< candidateChain chain2
        , candidateTipHeader = candidateTipHeader chain2
        , candidateScore = candidateScore chain1 + candidateScore chain2
        }

-- | O(1) Append the block /at the back/ of this 'ChainCandidate', which results
-- in a new tip.
snocBlock :: forall tx s. (Block tx s -> Score)
          -> Block tx s
          -> ChainCandidate s
          -> ChainCandidate s
snocBlock scoreBlock b cc =
    cc { candidateChain = candidateChain cc |> blockHash b
       , candidateTipHeader = blockHeader b
       , candidateScore = scoreBlock b + candidateScore cc
       }

{------------------------------------------------------------------------------
  Operations on the 'Orphanage'.
------------------------------------------------------------------------------}

-- | /O(1)/ Creates a new, empty 'Orphanage'.
emptyOrphanage :: (Block tx s -> Score) -> Orphanage tx s
emptyOrphanage = Orphanage mempty mempty mempty

getOrphans :: Orphanage tx s -> Set BlockHash
getOrphans = Map.keysSet . orphans

-- | /O(log(n))/ Picks the best candidate out of the candidate set at the given 'BlockHash'.
selectBestCandidate :: Eq s => BlockHash -> Orphanage tx s -> Maybe (ChainCandidate s)
selectBestCandidate blockOnMainChain o =
    case sortOn Down <$> Map.lookup blockOnMainChain (candidates o) of
        Nothing    -> Nothing
        Just []    -> Nothing
        Just (c:_) -> Just c

-- | /O(n * log(n))/ Given a list of block hashes on the main chain, selects the best-scoring
-- 'ChainCandidate' which would extend the chain from that given block.
selectBestChain :: Eq s => [BlockHash] -> Orphanage tx s -> Maybe (BlockHash, ChainCandidate s)
selectBestChain blocksOnMainChain o =
    case sortOn (Down . snd) . catMaybes $
         [(h,) <$> selectBestCandidate h o | h <- blocksOnMainChain] of
        []  -> Nothing
        c:_ -> Just c

-- | /O(m)/ Evicts the candidates branching off the input block hash from the 'Orphanage'.
bulkDelete :: BlockHash -> Orphanage tx s -> Orphanage tx s
bulkDelete bHash o =
    o { candidates = candidates' (candidates o)
      , tips = tips' (tips o)
      }
  where
      candidates' :: Map ChainRoot (Candidates s) -> Map ChainRoot (Candidates s)
      candidates' = Map.delete bHash

      tips' :: Map BlockHash (ChainRoot, Index) -> Map BlockHash (ChainRoot, Index)
      tips' oldTips =
          let updateFn mp staleChain =
                  case lookupChainTip staleChain of
                    Nothing -> mp
                    Just t  -> Map.delete t mp
          in case Map.lookup bHash (candidates o) of
              Nothing          -> oldTips
              Just staleChains -> foldl' updateFn oldTips staleChains

-- | /O(m)/ Inserts the candidates branching off the input block hash from the 'Orphanage'.
-- NOTE(adn) This function ought to be internal and just called /after/ the its dual
-- 'bulkDelete'. This is used inside 'insertOrphans' to prune and replace
-- candidates for which the parent block has changed.
bulkInsert :: forall tx s. ChainRoot -> Candidates s -> Orphanage tx s -> Orphanage tx s
bulkInsert ptr newChains o =
    o { candidates = candidates' (candidates o)
      , tips = tips' (tips o)
      }
  where
      candidates' :: Map ChainRoot (Candidates s) -> Map ChainRoot (Candidates s)
      candidates' = Map.insert ptr newChains

      tips' :: Map BlockHash (ChainRoot, Index) -> Map BlockHash (ChainRoot, Index)
      tips' oldTips =
          let updateFn mp (newChain, ix) =
                  case lookupChainTip newChain of
                    Nothing -> mp
                    Just t  -> Map.insert t (ptr, ix) mp
          in foldl' updateFn oldTips (zip newChains [0..])

-- | Stores the orphan block into the internal storage.
storeOrphan :: Block tx s -> Orphanage tx s -> Orphanage tx s
storeOrphan b o = o { orphans = Map.insert (blockHash b) b (orphans o) }

-- | Given a candidate which won, delete the orphans from the store and
-- the chain from the candidates
pruneOrphanage :: BlockHash -> ChainCandidate s -> Orphanage tx s -> Orphanage tx s
pruneOrphanage rootHash ChainCandidate{candidateChain} o =
    let o' = o { orphans = foldl' (flip Map.delete) (orphans o) (toList candidateChain) }
    in bulkDelete rootHash o'

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
-- FIXME(adn) Validate a block before insertion. Probably the way to go is
-- to have the 'Orphanage' take the score & validity function and remove this
-- from the BlockStore.
insertOrphan :: forall tx s. Block tx s -> Orphanage tx s -> Orphanage tx s
insertOrphan b (storeOrphan b -> o) =
  maybe o tryLinkChains (extendInFront <|> extendExisting)

  where
    bHash             = blockHash b
    parentHash        = Block.parentHash $ b
    currentCandidates = candidates o

    -- Check if this block is the parent of any existing chain, and if it is,
    -- this means we have to add it at the /front/ of
    -- of all the chain candidates that branch off from this block,
    -- as well as updating the outermost map.
    extendInFront :: Maybe (Orphanage tx s)
    extendInFront = do
        chains <- Map.lookup bHash currentCandidates
        let chains' = map (consBlock (scoreBlock o) b) chains
        pure $ bulkInsert parentHash chains' (bulkDelete bHash o)

    extendExisting :: Maybe (Orphanage tx s)
    extendExisting = tryExtendChain b o <|> addSingleton

    addSingleton :: Maybe (Orphanage tx s)
    addSingleton = Just $
        case Map.lookup parentHash currentCandidates of
          Just _existingCandidates ->
              o { candidates = Map.adjust (\old -> old <> [chainCandidate (scoreBlock o) b]) parentHash (candidates o)
                , tips       = Map.adjust (\(oldP, oldIx) -> (oldP, oldIx + 1))  parentHash (tips o)
                }
          Nothing ->
              -- This is genuinely a \"singleton\" chain, and we simply
              -- store it.
              insertSingletonChain parentHash b o

    -- If in the candidates we have the /parent/ of the block being inserted
    -- /and/ it also appears to be in the tips, we need to fuse two chains
    -- together.
    tryLinkChains :: Orphanage tx s -> Orphanage tx s
    tryLinkChains orph = fromMaybe orph $ do
        candidateChains <- Map.lookup parentHash (candidates orph)
        (chainPtr, ix)  <- Map.lookup parentHash (tips orph)
        chainToExtend   <- (List.!! ix) <$> Map.lookup chainPtr (candidates o)
        -- If we find a match, we need to "attach" all the blocks from the
        -- /chainToPrepend/ to the /chainToExtend/.
        let predFn c = ((bHash ==) <$> lookupChainRoot c) == Just True
        (chainToPrepend, ix') <- findCandidate predFn candidateChains
        chainTip <- lookupChainTip chainToPrepend
        let newChain = chainToExtend `consCandidates` chainToPrepend
        -- Finally, we need to remove the stale candidate(s).
        pure $ orph
            { candidates = Map.insert chainPtr (replaceAt ix' newChain candidateChains)
                         . Map.delete parentHash
                         $ candidates orph
            , tips = Map.adjust (const (chainPtr, ix')) chainTip
                   . Map.delete parentHash
                   $ tips orph
            }

findCandidate :: (ChainCandidate s -> Bool)
              -> Candidates s
              -> Maybe (ChainCandidate s, Index)
findCandidate predFn xs =
    case List.filter (\(c,_) -> predFn c) (zip xs [0..]) of
      [(c, ix)] -> Just (c, ix)
      _         -> Nothing

insertSingletonChain :: BlockHash
                     -> Block tx s
                     -> Orphanage tx s
                     -> Orphanage tx s
insertSingletonChain parentHash b o =
    o { candidates = Map.insert parentHash [chainCandidate (scoreBlock o) b] (candidates o)
      , tips       = Map.insert (blockHash b) (parentHash, 0) (tips o)
      }

-- | Tries extending an existing 'ChainCandidate' with the input block. It works
-- by looking up the /parent/ of the incoming block in the @tips@ map. If a
-- match is found, it means this block effectively extends one of the chains.
tryExtendChain :: Block tx s -> Orphanage tx s -> Maybe (Orphanage tx s)
tryExtendChain b o = do
    (parentBlockHash, candidateIndex) <- Map.lookup (blockPrevHash . blockHeader $ b) (tips o)
    candidateChain <- (List.!! candidateIndex) <$> Map.lookup parentBlockHash (candidates o)
    oldTip <- lookupChainTip candidateChain
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
replaceAt :: Int -> a -> [a] -> [a]
replaceAt ix new xs
  | ix >= length xs = xs
  | ix == 0 = new : drop 1 xs
  | otherwise =
    let (excludesXs, rest) = List.splitAt ix xs
    in excludesXs <> [new] <> drop ix rest
