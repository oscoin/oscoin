module Oscoin.Consensus.Tests (tests) where

import           Prelude (Show (..), (++), (+))

import           Oscoin.Prelude hiding ((+))
import           Oscoin.Consensus.Class

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import           Control.Monad.Reader
import           Data.List (sort, sortOn, nub)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (NominalDiffTime)

tests :: [TestTree]
tests = [ testProperty "All nodes include all txns" propNetworkNodesIncludeAllTxns ]

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 11

type TestMsg = Word8
type ScheduledMessage a = (Tick a, Addr a, Msg a)

data TestNode = TestNode (Addr TestNode) [Msg TestNode] [Addr TestNode]
    deriving (Eq, Ord, Show)

-- TODO(tyler): Better data structure for scheduled messages.
data TestNetwork = TestNetwork (Map (Addr TestNode) TestNode) [ScheduledMessage TestNode]
    deriving (Show)

instance Protocol TestNode where
    type Msg TestNode  = TestMsg
    type Addr TestNode = Word
    type Tick TestNode = NominalDiffTime

    step tn@(TestNode a state peers) _at (Just (from, msg))
        | msg `elem` state = (tn, [])
        | otherwise        = (TestNode a state' peers, broadcastMsgs)
      where
        state'        = sort (msg : state)
        filteredPeers = filter (/= from) peers
        broadcastMsgs = map (\p -> (p, msg)) filteredPeers
    step tn _at Nothing =
        (tn, [])

    epoch _ = 1

instance Arbitrary TestNetwork where
    arbitrary = do
        addrs <- Set.fromList <$> vectorOf kidSize arbitrary :: Gen (Set Word)

        nodes <- forM (toList addrs) $ \a ->
            pure (a, TestNode a [] [x | x <- toList addrs, x /= a])

        smsgs <- resize kidSize . listOf1 $ do
            msg   <- arbitrary :: Gen TestMsg
            dests <- sublistOf (toList addrs) :: Gen [Word]
            forM dests $ \d -> do
                at <- arbitrary :: Gen NominalDiffTime
                pure (at, d, msg)

        pure $ TestNetwork (Map.fromList nodes) (sortOn (\(x, _, _) -> x)  (mconcat smsgs))

runNetwork :: TestNetwork -> TestNetwork
runNetwork tn@(TestNetwork _nodes []) = tn
runNetwork (TestNetwork nodes ((tick, addr, msg):ms))
    | Just node <- Map.lookup addr nodes =
        let (node', msgs) = step node tick (Just (addr, msg))
            nodes'        = Map.insert addr node' nodes
            deliveryTime  = tick + 1
            scheduled     = [(deliveryTime, to, msg) | (to, msg) <- msgs]
            ms'           = sortOn (\(x, _, _) -> x) (scheduled ++ ms)
         in runNetwork (TestNetwork nodes' ms')
    | otherwise =
        runNetwork (TestNetwork nodes ms)

networkHasMessages :: TestNetwork -> Bool
networkHasMessages (TestNetwork _n []) = False
networkHasMessages _                   = True

propNetworkNodesIncludeAllTxns :: TestNetwork -> Property
propNetworkNodesIncludeAllTxns tn@(TestNetwork _nodes initialMsgs) =
    networkHasMessages tn ==>
        length (nub mappedInitialMsgs) == length (nub firstNodeMsgs)
        && length (nub nodeStates) == 1
  where
    mappedInitialMsgs = map (\(_, _, m) -> m) initialMsgs
    (TestNetwork nodes' _) = runNetwork tn
    (TestNode _addr firstNodeMsgs _peers) = head (toList nodes')
    -- Deduplicating the different node states should result in a single state.
    nodeStates = map (\(TestNode _addr' state _peers) -> state) (toList nodes')
