module Oscoin.Consensus.Tests (tests) where

import           Prelude (Show (..), (++), (+))

import           Oscoin.Prelude hiding ((+))
import           Oscoin.Consensus.Class

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import           Control.Monad.Reader
import           Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import           Data.List (sort, sortOn, nub)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (NominalDiffTime)

type DummyTx = Word8
type DummyState = [DummyTx]

newtype DummyView a = DummyView (State DummyState a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState DummyState
             )

runDummyView :: DummyView a -> DummyState -> (a, DummyState)
runDummyView (DummyView inner) s =
    runState inner s

instance Context DummyView where
    type T DummyView = DummyState
    type Key DummyView = ()

    get = notImplemented
    set = notImplemented
    del = notImplemented

instance View DummyView where
    type Transaction DummyView = DummyTx
    type BlockHeader DummyView = ()

    apply _ txs =
        for_ txs $ \tx ->
            State.modify (\s -> sort $ tx : s)

tests :: [TestTree]
tests = [ testProperty "All nodes include all txns" propNetworkNodesIncludeAllTxns ]

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 11

type TestMsg = Word8
type ScheduledMessage a = (Tick a, Addr a, Msg a)

data TestNode v = TestNode (Addr (TestNode v)) v [Addr (TestNode v)]

deriving instance Eq (TestNode DummyState)
deriving instance Ord (TestNode DummyState)
deriving instance Show (TestNode DummyState)

-- TODO(tyler): Better data structure for scheduled messages.
data TestNetwork a = TestNetwork
    { tnNodes :: Map (Addr a) a
    , tnMsgs  :: [ScheduledMessage a]
    }

deriving instance Show (TestNetwork (TestNode DummyState))

instance Protocol (TestNode DummyState) where
    type Msg  (TestNode DummyState) = DummyTx
    type Addr (TestNode DummyState) = Word
    type Tick (TestNode DummyState) = NominalDiffTime

    step tn@(TestNode a state peers) _at (Just (from, msg))
        | msg `elem` state = (tn, [])
        | otherwise        = (TestNode a state' peers, broadcastMsgs)
      where
        ((), state')  = runDummyView (apply Nothing [msg]) state
        filteredPeers = filter (/= from) peers
        broadcastMsgs = map (\p -> (p, msg)) filteredPeers
    step tn _at Nothing =
        (tn, [])

    epoch _ = 1

instance Arbitrary (TestNetwork (TestNode DummyState)) where
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

        pure $ TestNetwork
            { tnNodes = Map.fromList nodes
            , tnMsgs  = sortOn (\(x, _, _) -> x) (mconcat smsgs)
            }

runNetwork
    :: ( Ord (Addr a)
       , Protocol a
       ) => TestNetwork a -> TestNetwork a
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

networkHasMessages :: TestNetwork v -> Bool
networkHasMessages (TestNetwork _n []) = False
networkHasMessages _                   = True

propNetworkNodesIncludeAllTxns
    :: TestNetwork (TestNode DummyState) -> Property
propNetworkNodesIncludeAllTxns tn@(TestNetwork _nodes initialMsgs) =
    networkHasMessages tn ==>
        length (nub mappedInitialMsgs) == length (nub firstNodeMsgs)
            && length (nub nodeStates) == 1
  where
    mappedInitialMsgs = map (\(_, _, m) -> m) initialMsgs
    (TestNetwork nodes' _) = runNetwork tn
    (TestNode _addr firstNodeMsgs _peers) = head (toList nodes')
    -- Deduplicating the different node states should result in a single state.
    nodeStates = map (\(TestNode _ state _) -> state) (toList nodes')
