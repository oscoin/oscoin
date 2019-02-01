module Oscoin.Storage.Block.Orphanage where
    -- ( Orphanage
    -- , emptyOrphanage
    -- , insertOrphan
    -- , ChainCandidate
    -- ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (><), (|>))
import qualified Data.Sequence as Seq
import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain hiding (parentHash, (|>))

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type Index = Int
type ChainPointer = BlockHash
type Candidates   = [ChainCandidate]

data Orphanage tx s = Orphanage
    { orphans    :: Map BlockHash (Block tx s)
    -- ^ The /actual/ blocks, indexed by their block hash and in no particular
    -- order.
    , candidates :: Map ChainPointer Candidates
    -- ^ The chain candidates, indexed by the /parent hash/ of the /tail/ of
    -- the chain.
    , tips       :: Map BlockHash (ChainPointer, Index)
    -- ^ /All/ the tips of /all/ the chain candidates in the system. This
    -- allows fast lookup when we need to merge two chains together. The
    -- value indexed by this map is the 'BlockHash' to be used to lookup the
    -- candidates and the 'Index' of the particular candidate where the tip
    -- has to be found.
    }

data BlockWithScore tx s = BlockWithScore
    { bsBlock :: Block tx s
    , bsScore :: Score
    }

-- | A 'ChainCandidate' is a sequence of blocks and a partially-accumulated
-- 'Score, used to compute the best chain is 'selectBestCandidate'. The chain
-- is stored \"oldest first\", i.e. from the oldest to the newest block.
data ChainCandidate = ChainCandidate
    { candidateChain :: Seq BlockHash
    -- ^ The actual candidate chain, stored as a sequence of hashes.
    , candidateScore :: Score
    -- ^ The total 'Score' for this candidate
    } deriving (Eq, Show)

instance Ord ChainCandidate where
    compare chain1 chain2 =
        case candidateScore chain1 `compare` candidateScore chain2 of
          -- If we score a draw, pick the longest.
          EQ -> Seq.length (candidateChain chain1) `compare` Seq.length (candidateChain chain2)
          x  -> x

{------------------------------------------------------------------------------
  Operations on chain candidates
------------------------------------------------------------------------------}

-- | /O(1)/ Creates a new 'ChainCandidate' from the input 'BlockWithScore'.
newChainCandidate :: BlockWithScore tx s -> ChainCandidate
newChainCandidate bws = ChainCandidate
    { candidateChain = Seq.singleton (blockHash . bsBlock $ bws)
    , candidateScore = bsScore bws
    }

-- | /O(n * log(n))/. Returns a list of blocks out of this 'ChainCandidate',
-- ordered with the oldest first.
toBlocksOldestFirst :: Orphanage tx s -> ChainCandidate -> [Block tx s]
toBlocksOldestFirst Orphanage{orphans} ChainCandidate{candidateChain} =
    case sequence (map (`Map.lookup` orphans) (toList candidateChain)) of
      Nothing   -> panic "toBlocksOldestFirst: the candidate chain was empty."
      Just blks -> blks

-- | /O(1)/ Extracs the 'Score' out of this 'ChainCandidate'.
getCandidateScore :: ChainCandidate -> Score
getCandidateScore = candidateScore

-- | /O(1)/. Lookups the tip (i.e. the newest) block for this 'ChainCandidate'.
lookupChainTip :: ChainCandidate -> Maybe BlockHash
lookupChainTip cc = case Seq.viewr (candidateChain cc) of
                      EmptyR -> Nothing
                      _ :> b -> Just b

-- | /O(1)/. Lookups the root (i.e. the oldest) block for this 'ChainCandidate'.
lookupChainRoot :: ChainCandidate -> Maybe BlockHash
lookupChainRoot cc = case Seq.viewl (candidateChain cc) of
                      EmptyL -> Nothing
                      b :< _ -> Just b

-- | O(1) Prepend the block /in front/ of this 'ChainCandidate'. The tip stays
-- the same.
consBlock :: forall tx s. BlockWithScore tx s -> ChainCandidate -> ChainCandidate
consBlock BlockWithScore{..} cc =
    cc { candidateChain = blockHash bsBlock <| (candidateChain cc)
       , candidateScore = bsScore +  (candidateScore cc)
       }

-- | \( O(\log(\min(n_1,n_2))) \) Concatenate two candidates together, where
-- the second chain /extends/ the first one.
consCandidates :: ChainCandidate -> ChainCandidate -> ChainCandidate
consCandidates chain1 chain2 =
    ChainCandidate
        { candidateChain = candidateChain chain1 >< candidateChain chain2
        , candidateScore = candidateScore chain1 + candidateScore chain2
        }

-- | O(1) Append the block /at the back/ of this 'ChainCandidate', which results
-- in a new tip.
snocBlock :: forall tx s. BlockWithScore tx s -> ChainCandidate -> ChainCandidate
snocBlock BlockWithScore{..} cc =
    cc { candidateChain = (candidateChain cc) |> blockHash bsBlock
       , candidateScore = bsScore +  (candidateScore cc)
       }

{------------------------------------------------------------------------------
  Operations on the 'Orphanage'.
------------------------------------------------------------------------------}

-- | /O(1)/ Creates a new, empty 'Orphanage'.
emptyOrphanage :: Orphanage tx s
emptyOrphanage = Orphanage mempty mempty mempty

-- | Given a block on the main chain, selects the best-scoring
-- 'ChainCandidate' which would extend the chain from that given block.
selectBestCandidate :: BlockHash -> Orphanage tx s -> Maybe ChainCandidate
selectBestCandidate blockOnMainChain o =
    case sortBy (comparing Down) <$> Map.lookup blockOnMainChain (candidates o) of
        Nothing    -> Nothing
        Just []    -> Nothing
        Just (c:_) -> Just c

-- | /O(1)/ Evits the candidates branching off the input block hash from the 'Orphanage'.
bulkDelete :: BlockHash -> Orphanage tx s -> Orphanage tx s
bulkDelete bHash o =
    o { candidates = candidates' (candidates o)
      , tips = tips' (tips o)
      }
  where
      candidates' :: Map ChainPointer Candidates -> Map ChainPointer Candidates
      candidates' = Map.delete bHash

      tips' :: Map BlockHash (ChainPointer, Index) -> Map BlockHash (ChainPointer, Index)
      tips' oldTips =
          let updateFn mp staleChain =
                  case lookupChainTip staleChain of
                    Nothing -> mp
                    Just t  -> Map.delete t mp
          in case Map.lookup bHash (candidates o) of
              Nothing          -> oldTips
              Just staleChains -> foldl' updateFn oldTips staleChains

-- | /O(1)/ Inserts the candidates branching off the input block hash from the 'Orphanage'.
-- NOTE(adn) This function is ought to be internal and just called /after/ the its dual
-- 'deleteCandidates'. This is used inside 'insertOrphans' to prune and replace
-- candidates for which the parent block has changed.
bulkInsert :: ChainPointer -> Candidates -> Orphanage tx s -> Orphanage tx s
bulkInsert ptr newChains o =
    o { candidates = candidates' (candidates o)
      , tips = tips' (tips o)
      }
  where
      candidates' :: Map ChainPointer Candidates -> Map ChainPointer Candidates
      candidates' = Map.insert ptr newChains

      tips' :: Map BlockHash (ChainPointer, Index) -> Map BlockHash (ChainPointer, Index)
      tips' oldTips =
          let updateFn mp (newChain, ix) =
                  case lookupChainTip newChain of
                    Nothing -> mp
                    Just t  -> Map.insert t (ptr, ix) mp
          in foldl' updateFn oldTips (zip newChains [0..])

-- | Stores the orphan block into the internal storage.
storeOrphan :: BlockWithScore tx s -> Orphanage tx s -> Orphanage tx s
storeOrphan bws o =
    let bHash = blockHash . bsBlock $ bws
    in o { orphans = Map.insert bHash (bsBlock bws) (orphans o) }

-- | Adds a new 'Block' to the 'Orphanage'. Three scenarios are possible:
-- 1. This is a \"singleton\" 'Block', i.e. it doesn't extend any of the
--    existing chains. In this case, we simply insert it as a potential
--    candidate alongside the others.
-- 2. This block is the /parent/ of an existing set of candidates. In this
--    case, we iterate over all the candidates and add the block at the left
--    end of each sequence, as well as replacing the old, stale set with a
--    new one, which key now points to the parent hash of the incoming block.
-- 3. This block extends one of the chain candidates at the end (i.e. this is
--    a continuation of one of the chains. This is the trickiest scenario to
--    implement efficiently.
--
-- FIXME(adn) Validate a block before insertion. Probably the way to go is
-- to have the 'Orphanage' take the score & validity function and remove this
-- from the BlockStore.
insertOrphan :: forall tx s. BlockWithScore tx s -> Orphanage tx s -> Orphanage tx s
insertOrphan b (storeOrphan b -> o) =
    let orphanage' =
            -- Check if this block is the parent of any existing chain..
            case Map.lookup bHash currentCandidates of
              Just chains ->
                  -- Yes, it is. This means we have to add it at the /front/ of
                  -- of all the chain candidates that branch off from this block,
                  -- as well as updating the outermost map.
                  let chains' = map (consBlock b) chains
                  in bulkInsert parentHash chains' (bulkDelete bHash o)
              Nothing ->
                  -- No, it's not. This means either it's a \"singleton\" orphan
                  -- or it extends some other candidate we have already stored.
                  case tryExtendChain b o of
                     Just o' -> o'
                     Nothing ->
                         case Map.lookup parentHash currentCandidates of
                           Just _existingCandidates ->
                               o { candidates = Map.adjust (\old -> old <> [newChainCandidate b]) parentHash (candidates o)
                                 , tips       = Map.adjust (\(oldP, oldIx) -> (oldP, oldIx + 1))  parentHash (tips o)
                                 }
                           Nothing ->
                               -- This is genuinely a \"singleton\" chain, and we simply
                               -- store it.
                               insertSingletonChain parentHash b o
        in fromMaybe orphanage' (tryLinkChains orphanage')

  where
    bHash             = blockHash . bsBlock $ b
    parentHash        = blockPrevHash . blockHeader . bsBlock $ b
    currentCandidates = candidates o
    -- If in the candidates we have the /parent/ of the block being inserted
    -- /and/ it also appears to be in the tips, we need to fuse two chains
    -- together
    tryLinkChains :: Orphanage tx s -> Maybe (Orphanage tx s)
    tryLinkChains orph = do
        candidateChains <- Map.lookup parentHash (candidates orph)
        (chainPtr, ix)  <- Map.lookup parentHash (tips orph)
        chainToExtend   <- (List.!! ix) <$> (Map.lookup chainPtr (candidates o))
        -- If we find a match, we need to "attach" all the blocks from the
        -- /chainToPrepend/ to the /chainToExtend/.
        let predFn c = (((==) bHash) <$> lookupChainRoot c) == Just True
        (chainToPrepend, ix') <- findCandidate predFn candidateChains
        chainTip <- lookupChainTip chainToPrepend
        let newChain = chainToExtend `consCandidates` chainToPrepend
        -- Finally, we need to remove the stale candidate(s).
        pure $ orph
            { candidates = Map.insert chainPtr (replaceAt ix' newChain candidateChains)
                         . Map.delete parentHash
                         $ candidates orph
            , tips = Map.adjust (\_ -> (chainPtr, ix')) chainTip
                   . Map.delete parentHash
                   $ tips orph
            }

findCandidate :: (ChainCandidate -> Bool)
              -> Candidates
              -> Maybe (ChainCandidate, Index)
findCandidate predFn xs =
    case List.filter (\(c,_) -> predFn c) (zip xs [0..]) of
      [(c, ix)] -> Just (c, ix)
      _         -> Nothing

insertSingletonChain :: BlockHash
                     -> BlockWithScore tx s
                     -> Orphanage tx s
                     -> Orphanage tx s
insertSingletonChain parentHash bws o =
    o { candidates = Map.insert parentHash [newChainCandidate bws] (candidates o)
      , tips       = Map.insert (blockHash . bsBlock $ bws) (parentHash, 0) (tips o)
      }

-- | Tries extending an existing 'ChainCandidate' with the input block. It works
-- by looking up the /parent/ of the incoming block in the @tips@ map. If a
-- match is found, it means this block effectively extends one of the chains.
tryExtendChain :: BlockWithScore tx s -> Orphanage tx s -> Maybe (Orphanage tx s)
tryExtendChain b@BlockWithScore{bsBlock} o = do
    (parentBlockHash, candidateIndex) <- Map.lookup (blockPrevHash . blockHeader $ bsBlock) (tips o)
    candidateChain <- (List.!! candidateIndex) <$> (Map.lookup parentBlockHash (candidates o))
    oldTip <- lookupChainTip candidateChain
    -- We now extend the 'ChainCandidate' with the new block.
    let !chain' = snocBlock b candidateChain
    let candidates' = Map.adjust (replaceAt candidateIndex chain') parentBlockHash
                    $ (candidates o)
    let tips' = Map.insert (blockHash bsBlock) (parentBlockHash, candidateIndex)
              . Map.delete oldTip
              $ (tips o)
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

deleteAt :: Int -> [a] -> [a]
deleteAt ix xs
  | ix >= length xs = xs
  | ix == 0 = drop 1 xs
  | otherwise =
    let (excludes, rest) = List.splitAt ix xs
    in excludes <> drop ix rest


-- Temporary

debugShow :: Orphanage tx s -> String
debugShow Orphanage{..} = mconcat [
  "{ orphans = " <> show (Map.keys orphans) <>
  "\n, candidates = " <> show candidates <>
  "\n, tips = " <> show tips <>
  "\n}\n"
  ]
