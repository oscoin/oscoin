module Oscoin.Storage.Block.Orphanage
    ( Orphanage(..)
    , ChainCandidate
    ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq, ViewR(..), (<|), (|>))
import qualified Data.Sequence as Seq
import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain hiding ((|>))

-- | An 'Orphanage' stores a set of chain candidates implemented as a map
-- from the 'BlockHash' of the /tail/ of the candidate to the chain proper.
-- The choice of storing a chain in reverse order is such that we can compare
-- any incoming block to see if it's a \"missing link\" from one of the
-- candidates and the main (adopted) chain. If it is, we compare the overall
-- score of the chain candidate with the score of the main chain suffix and
-- performs a rollback-and-roll-forward if necessary.
newtype Orphanage tx s = Orphanage { orphans :: Map BlockHash (ChainCandidate tx s) }

newtype ChainCandidate tx s = ChainCandidate { candidateChain :: Seq (Block tx s) }

-- | Evits a 'ChainCandidate' from the 'Orphanage'.
evictChain :: BlockHash -> Orphanage tx s -> Orphanage tx s
evictChain bHash o = o { orphans = Map.delete bHash (orphans o) }

-- | Adds a new 'Block' to the 'Orphanage'. Three scenarios are possible:
-- 1. This is a \"singleton\" 'Block', i.e. it doesn't extend any of the
--    existing chains. In this case, we simply insert it.
-- 2. This 'Block' extends one of the 'ChainCandidate' at the end. In this
--    case is sufficient just to append at the end and update the entry in
--    the map (key unchanged).
-- 3. This 'Block' extends one of the 'ChainCandidate' at the front. In this
--    case we need to append at the front of the 'ChainCandidate' but also
--    evict the old (stale) chain in favour of this new one.
addOrphan :: forall tx s. Block tx s -> Orphanage tx s -> Orphanage tx s
addOrphan b o =
    case Map.lookup (blockPrevHash . blockHeader $ b) (orphans o) of
        Nothing ->
            -- Check for Scenario 3
            case List.find lookupEnd (Map.toList (orphans o)) of
                Nothing ->
                  -- Scenario 1. This is a \"singleton\" orphan.
                  o { orphans = Map.insert (blockHash b) (ChainCandidate $ Seq.singleton b) (orphans o) }
                Just (key, ChainCandidate existingCandidate) ->
                    -- Scenario 3
                    let newChain = ChainCandidate (existingCandidate |> b)
                    in  o { orphans = Map.insert key newChain (orphans o) }
        Just (ChainCandidate existingCandidate) ->
            -- Scenario 2.
            let newChain = ChainCandidate (b <| existingCandidate)
                o' = evictChain (blockPrevHash . blockHeader $ b) o
            in  o { orphans = Map.insert (blockHash b) newChain (orphans o') }
  where
      lookupEnd :: (BlockHash, ChainCandidate tx s) -> Bool
      lookupEnd (_, ChainCandidate ck) = case Seq.viewr ck of
          EmptyR  -> False
          _ :> b' -> (blockPrevHash . blockHeader $ b) == blockHash b'
