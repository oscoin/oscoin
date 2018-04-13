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

tests :: [TestTree]
tests = [ testProperty "All nodes include all txns" (propNetworkNodesIncludeAllTxns @(TestNode DummyState)) ]

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 11

-- TNode ----------------------------------------------------------------------

class (Eq (TNodeTx a), Protocol a) => TNode a where
    type TNodeTx a :: *

    nodeState :: a -> [TNodeTx a]
    isResting :: a -> Bool

-- DummyView ------------------------------------------------------------------

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

-- TestNode -------------------------------------------------------------------

data TestNode v = TestNode (Addr (TestNode v)) v [Addr (TestNode v)]

deriving instance Eq (TestNode DummyState)
deriving instance Ord (TestNode DummyState)
deriving instance Show (TestNode DummyState)

-- TODO(alexis): Try to make this `Protocol (TestNode s)`
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

instance TNode (TestNode DummyState) where
    type TNodeTx (TestNode DummyState) = DummyTx

    nodeState (TestNode _ s _) = s
    isResting _ = True

-- BufferedTestNode -----------------------------------------------------------

data BufferedTestNode s = BufferedTestNode
    { btnAddr    :: Addr (BufferedTestNode s)
    , btnPeers   :: [Addr (BufferedTestNode s)]
    , btnBuffer  :: [Msg (BufferedTestNode s)]
    , btnTick    :: Tick (BufferedTestNode s)
    , btnState   :: s
    }

instance Protocol (BufferedTestNode DummyState) where
    type Msg  (BufferedTestNode DummyState) = DummyTx
    type Addr (BufferedTestNode DummyState) = Word
    type Tick (BufferedTestNode DummyState) = NominalDiffTime

    step btn@BufferedTestNode{..} _ (Just (_, msg))
        | msg `elem` btnState =
            (btn, [])
        | otherwise =
            (btn { btnState = state', btnBuffer = msg : btnBuffer }, [])
      where
        ((), state')  = runDummyView (apply Nothing [msg]) btnState

    step btn@BufferedTestNode{..} tick Nothing
        | tick - btnTick > epoch btn =
            (btn { btnTick = tick, btnBuffer = [] }, outgoing)
        | otherwise =
            (btn, [])
      where
        outgoing = [(p, msg) | msg <- btnBuffer, p <- btnPeers]

    epoch _ = 1

-- TestNetwork ----------------------------------------------------------------

type ScheduledMessage a = (Tick a, Addr a, Maybe (Msg a))

-- TODO(tyler): Better data structure for scheduled messages.
data TestNetwork a = TestNetwork
    { tnNodes :: Map (Addr a) a
    , tnMsgs  :: [ScheduledMessage a]
    }

deriving instance Show (TestNetwork (TestNode DummyState))

instance Arbitrary (TestNetwork (TestNode DummyState)) where
    arbitrary = do
        addrs <- Set.fromList <$> vectorOf kidSize arbitrary :: Gen (Set Word)

        nodes <- forM (toList addrs) $ \a ->
            pure (a, TestNode a [] [x | x <- toList addrs, x /= a])

        smsgs <- resize kidSize . listOf1 $ do
            msg   <- arbitrary :: Gen (Msg (TestNode DummyState))
            dests <- sublistOf (toList addrs) :: Gen [Word]
            forM dests $ \d -> do
                at <- arbitrary :: Gen NominalDiffTime
                pure (at, d, Just msg)

        pure $ TestNetwork
            { tnNodes = Map.fromList nodes
            , tnMsgs  = sortOn (\(x, _, _) -> x) (mconcat smsgs)
            }

runNetwork
    :: ( Ord (Addr a)
       , TNode a
       ) => TestNetwork a -> TestNetwork a
runNetwork tn@(TestNetwork nodes [])
    | all isResting nodes = tn
    | otherwise           = runNetwork tn
runNetwork (TestNetwork nodes ((tick, addr, Nothing) : ms)) =
    runNetwork (scheduleMessages tick msgs tn)
  where
    (tn, msgs) = deliverTick tick addr (TestNetwork nodes ms)
runNetwork (TestNetwork nodes ((tick, addr, Just msg) : ms)) =
    runNetwork (scheduleMessages tick msgs tn)
  where
    (tn, msgs) = deliverMessage tick (addr, msg) (TestNetwork nodes ms)

deliverTick
    :: (TNode a, Ord (Addr a))
    => Tick a -> Addr a -> TestNetwork a -> (TestNetwork a, [(Addr a, Msg a)])
deliverTick tick addr tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup addr tnNodes =
        let (node', msgs) = step node tick Nothing
            nodes'        = Map.insert addr node' tnNodes
         in (tn { tnNodes = nodes' }, msgs)
    | otherwise =
        (tn, [])

deliverMessage
    :: (TNode a, Ord (Addr a))
    => Tick a -> (Addr a, Msg a) -> TestNetwork a -> (TestNetwork a, [(Addr a, Msg a)])
deliverMessage tick (addr, msg) tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup addr tnNodes =
        let (node', msgs) = step node tick (Just (addr, msg))
            nodes'        = Map.insert addr node' tnNodes
         in (tn { tnNodes = nodes' }, msgs)
    | otherwise =
        (tn, [])

scheduleMessages
    :: (Ord (Addr a), TNode a)
    => Tick a -> [(Addr a, Msg a)] -> TestNetwork a -> TestNetwork a
scheduleMessages t msgs tn =
    let deliveryTime  = t + 1
        scheduled     = [(deliveryTime, to, Just msg) | (to, msg) <- msgs]
        msgs'         = sortOn (\(x, _, _) -> x) (scheduled ++ tnMsgs tn)
     in runNetwork tn { tnMsgs = msgs' }

networkHasMessages :: TestNetwork v -> Bool
networkHasMessages (TestNetwork _n []) = False
networkHasMessages _                   = True

-------------------------------------------------------------------------------

propNetworkNodesIncludeAllTxns
    :: (Eq (Msg a), Ord (Addr a), TNode a)
    => TestNetwork a -> Property
propNetworkNodesIncludeAllTxns tn@(TestNetwork _nodes initialMsgs) =
    networkHasMessages tn ==>
        length (nub mappedInitialMsgs) == length (nub firstNodeMsgs)
            && length (nub nodeStates) == 1
  where
    mappedInitialMsgs = map (\(_, _, m) -> m) initialMsgs
    TestNetwork nodes' _ = runNetwork tn
    firstNodeMsgs = nodeState $ head (toList nodes')
    -- Deduplicating the different node states should result in a single state.
    nodeStates = map nodeState (toList nodes')
