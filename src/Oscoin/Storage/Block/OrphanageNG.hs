{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Storage.Block.OrphanageNG
    ( Orphanage -- opaque
    , emptyOrphanage
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

    -- * Internal and test use only
    , getOrphans
    , sizeAt
    , candidates
    , ChainCandidate(..)
    , ChainMeasure(..)
    ) where

import           Oscoin.Prelude
import qualified Prelude

import           Oscoin.Crypto.Blockchain hiding (parentHash, (|>))
import qualified Oscoin.Crypto.Blockchain as Block
import           Oscoin.Crypto.Blockchain.Block (SealedBlock)
import           Oscoin.Crypto.Hash (HasHashing, Hash, zeroHash)
import           Oscoin.Time.Chrono

import           Data.Coerce (coerce)
import           Data.Heap (Heap)
import qualified Data.Heap as Heap
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.RefCounted (RefCounted)
import qualified Data.Map.RefCounted as RefCount
import qualified Data.Map.Strict as Map
import           Data.Ord (Down)
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type ChainRoot c = BlockHash c

data ChainCandidate c tx s = ChainCandidate
    { ccChain   :: Seq (SealedBlock c tx s)
    , ccMeasure :: ChainMeasure c
    -- ^ A cached measure for this candidate.
    }

instance (Eq (ChainMeasure c), HasHashing c) => Eq (ChainCandidate c tx s) where
    c1 == c2 = ccMeasure c1 == ccMeasure c2

deriving instance ( Show tx
                  , Show s
                  , Show (Beneficiary c)
                  , Show (Hash c)
                  ) => Show (ChainCandidate c tx s)

newtype Candidates c tx s = Candidates
    { getCandidates :: Heap (Down (ChainCandidate c tx s)) }

deriving instance Show (ChainCandidate c tx s) => Show (Candidates c tx s)

data Orphanage c tx s = Orphanage
    { orphans    :: RefCounted (BlockHash c) (SealedBlock c tx s)
    -- ^ The actual orphans.
    , candidates :: Map (ChainRoot c) (Candidates c tx s)
    -- ^ The chain candidates, indexed by the /parent hash/ of the /root/ of
    -- the chain (i.e. the oldest block and the first in the sequence).
    , scoreBlock :: SealedBlock c tx s -> Score
    -- ^ A function to assign a 'Score' to a block.
    }

-- Bogus instance created only to please the test code.
instance ( Show tx
         , Show s
         , Show (Hash c)
         , Show (Beneficiary c)
         ) => Show (Orphanage c tx s) where
    show Orphanage{..} = "Orphanage { orphans = " ++ show orphans ++
                         "          , candidates = " ++ show candidates ++
                         "          }"

-- | A 'ChainMeasure' stores a partially-accumulated 'Score', used to
-- compute the best chain in 'selectBestChain', as well as
-- other useful information we want to cache.
data ChainMeasure c = ChainMeasure
    { mScore  :: Score
      -- ^ The total 'Score' for this candidate
    , mRoot   :: BlockHash c
      -- ^ The hash of the root block for the candidate, i.e. the 'blockPrevHash'
      -- of the oldest block of this candidate.
    , mOldest :: BlockHash c
      -- ^ the hash of the oldest block of this candidate.
    , mTip    :: BlockHash c
      -- ^ The current tip for the candidate.
    }

instance Semigroup (ChainMeasure c) where
    (ChainMeasure s1 r1 o1 _t1) <> (ChainMeasure s2 _r2 _o2 t2) =
        ChainMeasure (s1 + s2) r1 o1 t2

instance HasHashing c => Monoid (ChainMeasure c) where
    mempty = ChainMeasure 0 zeroHash zeroHash zeroHash
    mappend = (<>)

deriving instance Show (Hash c) => Show (ChainMeasure c)
deriving instance Eq (Hash c)   => Eq (ChainMeasure c)

instance ( HasHashing c
         , Ord (Hash c)
         , Eq s
         , Eq (ChainMeasure c)
         ) => Ord (ChainCandidate c tx s) where
    compare (ChainCandidate c1 m1) (ChainCandidate c2 m2) =
        let compareScore  = mScore m1 `compare` mScore m2
            compareLength = length c1 `compare` length c2
            compareTipHash = mTip m1 `compare` mTip m2
        -- If we score a draw, pick the longest.  If we draw a score /again/,
        -- compare the hash of the tip header.
        in mconcat [compareScore, compareLength, compareTipHash]

{------------------------------------------------------------------------------
  Operations on 'Candidates' and 'ChainCandidate'
------------------------------------------------------------------------------}

-- | /O(1)/ The number of chain candidates for this 'Candidates' collection.
candidatesSize :: Candidates c tx s -> Int
candidatesSize (Candidates h) = Heap.size h

-- | /O(1)/ Creates a new 'ChainCandidate' from the input 'SealedBlock'.
chainCandidate
    :: (SealedBlock c tx s -> Score)
    -> SealedBlock c tx s
    -> ChainCandidate c tx s
chainCandidate scoreBlock b =
    let m = ChainMeasure { mScore = scoreBlock b
                         , mRoot  = blockPrevHash $ blockHeader b
                         , mOldest = blockHash b
                         , mTip = blockHash b
                         }
    in ChainCandidate (Seq.singleton b) m

-- | /O(n)/ Builds a 'ChainCandidate' out of a list of blocks. Very useful to turn
-- a chain suffix on the main chain into a 'ChainCandidate' for comparison,
-- to determine which of the two won.
fromChainSuffix
    :: HasHashing c
    => (SealedBlock c tx s -> Score)
    -> OldestFirst NonEmpty (SealedBlock c tx s)
    -> ChainCandidate c tx s
fromChainSuffix scoreBlock (toOldestFirst -> blks) =
    case blks of
      b :| bs -> foldl' (\acc blk -> appendBlock (blk, scoreBlock blk) acc)
                        (chainCandidate scoreBlock b)
                        bs

-- | /O(n)/. Returns a list of blocks out of this 'ChainCandidate',
-- ordered with the oldest first.
toBlocksOldestFirst
    :: ChainCandidate c tx s
    -> OldestFirst NonEmpty (SealedBlock c tx s)
toBlocksOldestFirst =
    OldestFirst . NonEmpty.fromList
                . toList
                . ccChain

-- | /O(1)/. Looks up the tip (i.e. the newest) block for this 'ChainCandidate'.
chainTip
    :: ChainCandidate c tx s
    -> BlockHash c
chainTip = mTip . ccMeasure

-- | /O(1)/. Looks up the root (i.e. the oldest) block for this 'ChainCandidate'.
chainRoot
    :: ChainCandidate c tx s
    -> BlockHash c
chainRoot = mRoot . ccMeasure

-- | /O(1)/. Looks up the oldest block for this 'ChainCandidate'.
chainOldest
    :: ChainCandidate c tx s
    -> BlockHash c
chainOldest = mOldest . ccMeasure

-- | /O(1)/ Append the block /at the back/ of this 'ChainCandidate', which results
-- in a new tip.
appendBlock
    :: HasHashing c
    => (SealedBlock c tx s, Score)
    -> ChainCandidate c tx s
    -> ChainCandidate c tx s
appendBlock bws@(b,_) (ChainCandidate cc m) =
    ChainCandidate (cc Seq.|> b) (updateChainMeasure bws m)

-- | /O(1)/ Prepend the block /in front/ of this 'ChainCandidate'. The tip stays
-- the same.
prependBlock
    :: HasHashing c
    => (SealedBlock c tx s, Score)
    -> ChainCandidate c tx s
    -> ChainCandidate c tx s
prependBlock bws@(b,_) (ChainCandidate cc m) =
    ChainCandidate (b Seq.<| cc) (updateChainMeasure bws m)

updateChainMeasure
    :: HasHashing c
    => (SealedBlock c tx s, Score)
    -> ChainMeasure c
    -> ChainMeasure c
updateChainMeasure (b, score) m@ChainMeasure{..} =
    if m == mempty
       then ChainMeasure
            { mScore  = score
            , mRoot   = blockPrevHash $ blockHeader b
            , mOldest = blockHash b
            , mTip    = blockHash b
            }
       else ChainMeasure
            { mScore  = mScore + score
            , mRoot   = if mRoot   == blockHash b then blockParent else mRoot
            , mOldest = if mOldest == blockParent then mOldest else blockHash b
            , mTip    = if mTip    == blockParent then blockHash b else mTip
            }
  where
    blockParent = blockPrevHash (blockHeader b)

-- | \( O(\log(\min(n_1,n_2))) \) Tries to concatenate two candidates together.
-- The order of the argument doesn't matter, as long as one is a valid
-- extension of the other.
extendChain
    :: HasHashing c
    => ChainCandidate c tx s
    -> ChainCandidate c tx s
    -> Maybe (ChainCandidate c tx s)
extendChain (ChainCandidate c1 m1) (ChainCandidate c2 m2) =
    if | mTip m1 == mRoot m2 -> Just $ ChainCandidate (c1 >< c2) (m1 <> m2)
       | mTip m2 == mRoot m1 -> Just $ ChainCandidate (c2 >< c1) (m2 <> m1)
       | otherwise -> Nothing

-- | Tries to extend the first candidates with the second ones.
extendCandidates
    :: forall c tx s.
       ( HasHashing c
       , Eq s
       )
    => (SealedBlock c tx s -> Score)
    -> Candidates c tx s
    -> (SealedBlock c tx s, Candidates c tx s)
    -> Maybe (Candidates c tx s)
extendCandidates scoreBlock (Candidates candidatesToExtend) (lastInsertedBlock, Candidates c2) =
    let c' = Heap.fromList $ concatMap tryExtend candidatesToExtend
    in if c' /= candidatesToExtend then Just (Candidates c') else Nothing
  where
      tryExtend
          :: Down (ChainCandidate c tx s) -> [Down (ChainCandidate c tx s)]
      tryExtend candidate@(Down chain) =
        let parentChain = splitChainAt scoreBlock validExtension chain
            newCandidates = catMaybes $
                map (extendChain parentChain . coerce) (toList c2)
        in case parentChain of
             x | chainTip x == blockParent ->
                 -- Here we are extending directly at the tip, which means the
                 -- old @candidate@ is not valid anymore and it must not be
                 -- included in the final result set.
                 if | chainTip x == chainTip chain -> map Down newCandidates
                    -- Possible insertion in-the-middle. Preserve the original
                    -- candidate.
                    | otherwise -> candidate : map Down newCandidates
             _                             -> [candidate]

      validExtension :: SealedBlock c tx s -> Bool
      validExtension b = blockHash b == blockParent

      blockParent :: BlockHash c
      blockParent = blockPrevHash (blockHeader lastInsertedBlock)

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
sizeAt rootHash (Orphanage _ chains _) =
    case Map.lookup rootHash chains of
      Nothing -> 0
      Just cs -> length (getCandidates cs)

-- | /O(1)/ Creates a new, empty 'Orphanage'.
emptyOrphanage
    :: Ord (Hash c)
    => (Block c tx (Sealed c s) -> Score)
    -> Orphanage c tx s
emptyOrphanage = Orphanage RefCount.empty mempty

-- /O(n)/
getOrphans :: Orphanage c tx s -> Set (BlockHash c)
getOrphans = RefCount.keysSet . orphans

member :: Ord (BlockHash c) => Orphanage c tx s -> BlockHash c -> Bool
member o h = RefCount.member h . orphans $ o

-- | /O(1)/ The size of the 'Orphanage', i.e. how many /unique/ orphans are
-- currently stored.
size :: Orphanage c tx s -> Int
size = RefCount.size . orphans

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
    :: HasHashing c
    => BlockHash c
    -> Orphanage c tx s
    -> Maybe (ChainCandidate c tx s)
selectBestCandidate blockOnMainChain o = do
    chains <- getCandidates <$> Map.lookup blockOnMainChain (candidates o)
    if Heap.null chains
       then Nothing
       else Just . coerce $ Heap.minimum chains

-- | /O(n * log(n))/ Given a list of block hashes on the main chain,
-- selects the best-scoring 'ChainCandidate' which would extend the chain
-- from that given block.
selectBestChain
    :: ( HasHashing c
       , Eq s
       )
    => [BlockHash c]
    -> Orphanage c tx s
    -> Maybe (BlockHash c, ChainCandidate c tx s)
selectBestChain blocksOnMainChain o =
    case sortOn (Down . snd) . catMaybes $
         [(h,) <$> selectBestCandidate h o | h <- blocksOnMainChain] of
        []  -> Nothing
        c:_ -> Just c

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
       )
    => ChainCandidate c tx s
    -> Orphanage c tx s
    -> Orphanage c tx s
pruneOrphanageDeep chain o =
    case Map.lookup (chainRoot chain) (candidates o) of
      Nothing    -> o
      Just currentCandidates ->
          let (counts, candidates') = prune currentCandidates
          in o { candidates = Map.insert (chainRoot chain) candidates' (candidates o)
               , orphans = Map.foldlWithKey' (\acc rootHash occurrences ->
                   foldl' (\acc' _ -> RefCount.delete rootHash acc') acc [1 .. occurrences]
                                      ) (orphans o) counts
               }

  where
    -- Prunes from the candidates tree the candidates which have as parent
    -- the @mOldest@ of the chain being pruned (this includes the input chain itself).
    -- Returns the updated 'Candidates' together with an occurence map to be
    -- used to decrease the reference count of @orphans@.
    prune :: Candidates c tx s -> (Map (BlockHash c) Int, Candidates c tx s)
    prune (Candidates cands) =
        case Heap.partition (\(Down cc) -> chainOldest cc == chainOldest chain) cands of
            (satisfy, doesNotSatisfy) ->
                ( foldl' occurrenceMap mempty satisfy
                , Candidates $ doesNotSatisfy
                )

    occurrenceMap
        :: Map (BlockHash c) Int
        -> Down (ChainCandidate c tx s)
        -> Map (BlockHash c) Int
    occurrenceMap acc (Down (ChainCandidate blks _)) =
        foldl' (\mp b ->
                   Map.alter (maybe (Just 1) (Just . succ)) (blockHash b) mp
               ) acc blks


-- | Given a candidate which won, delete the orphans from the store and
-- the chain from the candidates.
-- NOTE(adn): This doesn't "promote" any dangling fork originating from
-- pruning the chain into standalone candidates.
pruneOrphanageShallow
    :: forall c tx s.
       ( Ord (BlockHash c)
       , HasHashing c
       )
    => ChainCandidate c tx s
    -> Orphanage c tx s
    -> Orphanage c tx s
pruneOrphanageShallow chain@(ChainCandidate blks m) o =
    let prune = Candidates . Heap.filter (Down chain /=) . getCandidates
    in o { candidates = Map.adjust prune (mRoot m) (candidates o)
         , orphans    = foldl' (\acc b -> RefCount.delete (blockHash b) acc)
                               (orphans o)
                               blks
         }

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
    :: forall c tx s.
       ( Ord (BlockHash c)
       , HasHashing c
       , Eq s
       )
    => Block c tx (Sealed c s)
    -> Orphanage c tx s
    -> Orphanage c tx s
insertOrphan b o = withFrozenCallStack $
    if not (member o blkHash) then fromMaybe o (extendExisting <|> newCandidate)
                              else o

  where
    blkHash           = blockHash b
    parentHash        = Block.parentHash $ b
    currentCandidates = candidates o
    bws               = (b, scoreBlock o b)

    extendExisting :: Maybe (Orphanage c tx s)
    extendExisting = extendInFront <|> extendAtBack <|> extendInTheMiddle

    -- Check if this block is the parent of any existing chain, and if it is,
    -- this means we have to add it at the /front/ of
    -- of all the chain candidates that branch off from this block,
    -- as well as updating the outermost map.
    extendInFront :: Maybe (Orphanage c tx s)
    extendInFront = do
        chains' <- insertAtFront bws <$> Map.lookup blkHash currentCandidates
        let candidates' = currentCandidates
                        & Map.delete blkHash
                        & Map.insert parentHash chains'

        -- If the incoming block extended the candidates at the front, it means
        -- we might need to fuse two chains together.
        pure $ tryLinkChains b $ o {
                   candidates = candidates'
                   -- Increment the ref-count by the number of current
                   -- candidates for blockHash.
                 , orphans    =
                     foldl' (\acc _ -> RefCount.insert blkHash b acc)
                            (orphans o)
                            [1 .. candidatesSize chains']
                 }

    -- /O(n * m)/ in the worst-case scenario.
    extendAtBack :: Maybe (Orphanage c tx s)
    extendAtBack = do
        let predFn (Down (ChainCandidate _ m)) = mTip m == parentHash
        (rootHash, candidates') <- fst $
            Map.mapAccumWithKey (\acc root cnds ->
                  (acc <|> map (root,) (insertAtBack predFn bws cnds), cnds)
                ) Nothing currentCandidates
        pure $ o { candidates = Map.insert rootHash candidates' currentCandidates
                 , orphans    = RefCount.insert blkHash b (orphans o)
                 }

    -- /O(n * m)/ in the worst-case scenario.
    extendInTheMiddle :: Maybe (Orphanage c tx s)
    extendInTheMiddle = do
        let predFn = (==) parentHash . blockHash
        (rootHash, candidates') <- fst $
            Map.mapAccumWithKey (\acc root cnds ->
                  (acc <|> map (root,) (insertAt (scoreBlock o) predFn b cnds), cnds)
                ) Nothing currentCandidates
        pure $ o { candidates = Map.insert rootHash candidates' currentCandidates
                 , orphans    = RefCount.insert blkHash b (orphans o)
                 }

    newCandidate :: Maybe (Orphanage c tx s)
    newCandidate = do
        let new = chainCandidate (scoreBlock o) b
        pure o { candidates =
            Map.alter (\mbC -> Just $ case mbC of
                                  Nothing -> Candidates $ Heap.singleton (Down new)
                                  Just c  -> insertSingleton new c
                       ) parentHash currentCandidates
          , orphans = RefCount.insert blkHash b (orphans o)
          }

tryLinkChains
    :: ( HasHashing c
       , Eq s
       )
    => SealedBlock c tx s
    -> Orphanage c tx s
    -> Orphanage c tx s
tryLinkChains b o = fromMaybe o $ do
    childrenChains <- Map.lookup parentHash (candidates o)
    let candidates' = Map.delete parentHash (candidates o)
    let refCount   = Heap.size . getCandidates $ childrenChains

    -- Fused are now the 'Candidates' that includes also all the 'childrenChains',
    -- which are now stale.
    fused <- fst $
        Map.mapAccumWithKey (\acc root cnds ->
              (acc <|> map (root,) (extendCandidates (scoreBlock o) cnds (b, childrenChains)), cnds)
            ) Nothing candidates'

    pure $ o { candidates = Map.insert (fst fused) (snd fused) candidates'
             , orphans    =
                 -- FIXME(adn) This is not correct.
                 foldl' (\acc _ -> RefCount.insert (blockHash b) b acc)
                        (orphans o)
                        [1 .. refCount]
             }
  where
      parentHash = blockPrevHash $ blockHeader b


-- | /O(i)/ Iterates through all the Candidates and prepend the input block.
insertAtFront
    :: forall c tx s.
       ( HasHashing c
       , Eq s
       )
    => (SealedBlock c tx s, Score)
    -> Candidates c tx s
    -> Candidates c tx s
insertAtFront bws (Candidates allCandidates) =
  Candidates $ Heap.map extend allCandidates
  where
    extend :: Down (ChainCandidate c tx s) -> Down (ChainCandidate c tx s)
    extend (Down chain) = Down (prependBlock bws chain)

-- | /O(1)/ Insert a singleton 'ChainCandidate' into the 'Candidates'.
insertSingleton
    :: ( HasHashing c
       , Eq s
       )
    => ChainCandidate c tx s
    -> Candidates c tx s
    -> Candidates c tx s
insertSingleton candidate (Candidates allCandidates) =
  Candidates $ Heap.insert (Down candidate) allCandidates

-- | /O(n)/ Iterates through all the Candidates and append the input block if
-- the predicate on the 'ChainCandidate' returns 'True'.
insertAtBack
    :: forall c tx s.
       ( HasHashing c
       , Eq s
       )
    => (Down (ChainCandidate c tx s) -> Bool)
    -> (SealedBlock c tx s, Score)
    -> Candidates c tx s
    -> Maybe (Candidates c tx s)
insertAtBack predFn bws (Candidates allCandidates) =
    case Heap.partition predFn allCandidates of
      -- A block can extend at max one candidate, so the size of @satisfy@
      -- must be 1.
      (satisfy, doesNotSafisty) | Heap.size satisfy == 1 ->
          Just (Candidates $ Heap.map extend satisfy `Heap.union` doesNotSafisty)
      _ -> Nothing
  where
    extend :: Down (ChainCandidate c tx s) -> Down (ChainCandidate c tx s)
    extend (Down chain) = Down (appendBlock bws chain)

-- | /O(i)/ Given a predicate, it returns the possibly empty 'ChainCandidate'
-- such that the predicate returned 'True', element included.
-- >>> splitChainAt (\b -> blockHash b == "a") (candidateFromList $ [Block "a" "genesis" 10, Block "b" "a" 20])
-- ChainCandidate [Block "a" "genesis" 10] (ChainMeasure 10 "genesis" "a")
splitChainAt
    :: forall c tx s. HasHashing c
    => (SealedBlock c tx s -> Score)
    -> (SealedBlock c tx s -> Bool)
    -> ChainCandidate c tx s
    -> ChainCandidate c tx s
splitChainAt scoreFn predFn (ChainCandidate cc _) =
    go cc (ChainCandidate mempty mempty)
  where
    go :: Seq (SealedBlock c tx s) -> ChainCandidate c tx s -> ChainCandidate c tx s
    go s acc =
        case Seq.viewl s of
          Seq.EmptyL -> acc
          b Seq.:< rest ->
              if predFn b then appendBlock (b, scoreFn b) acc
                          else go rest (appendBlock (b, scoreFn b) acc)

-- | /O(n)/ Iterates through all the Candidates and tries to insert the input
-- Block as a children of an existing candidate, generating another candidate.
insertAt
    :: forall c tx s.
       ( HasHashing c
       , Eq s
       )
    => (SealedBlock c tx s -> Score)
    -> (SealedBlock c tx s -> Bool)
    -> SealedBlock c tx s
    -> Candidates c tx s
    -> Maybe (Candidates c tx s)
insertAt scoreBlock predFn children (Candidates allCandidates) =
    let c' = Heap.fromList $ concatMap tryExtend allCandidates
    in if Heap.size c' /= Heap.size allCandidates
          then Just (Candidates c') else Nothing
  where
    tryExtend :: Down (ChainCandidate c tx s) -> [Down (ChainCandidate c tx s)]
    tryExtend candidate@(Down chain) =
        let parentChain = splitChainAt scoreBlock predFn chain
            blockParent = blockPrevHash . blockHeader $ children
            bws = (children, scoreBlock children)
        in case chainTip parentChain of
          x | x == blockParent ->
               [candidate, Down (appendBlock bws parentChain)]
          _ -> [candidate]
