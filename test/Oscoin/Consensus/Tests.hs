module Oscoin.Consensus.Tests (tests) where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import           Control.Monad.Reader
import qualified Control.Monad.State as State
import           Control.Monad.State (State)
import           Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Time.Clock (NominalDiffTime)

tests :: [TestTree]
tests = [ testProperty "All nodes include all txns"            (propNetworkNodesIncludeAllTxns @(TestNode DummyTx))
        , testProperty "All nodes include all txns (buffered)" (propNetworkNodesIncludeAllTxns @(BufferedTestNode DummyTx)) ]

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 11

-- TestableNode ---------------------------------------------------------------

class ( Eq (TestableTx a)
      , Ord (Scheduled a)
      , Ord (Addr a)
      , Arbitrary (Msg a)
      , Arbitrary (Addr a)
      , Protocol a ) => TestableNode a where
    type TestableTx a :: *

    testableNode :: Addr a -> [Addr a] -> a
    testableNodeState :: a -> [TestableTx a]
    isResting :: a -> Bool

-- DummyView ------------------------------------------------------------------

type DummyTx = Word8

newtype DummyView (f :: * -> *) tx a = DummyView (State (f tx) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState (f tx)
             )

runDummyView :: DummyView f tx a -> f tx -> (a, f tx)
runDummyView (DummyView inner) xs =
    runState inner xs

instance Context (DummyView f tx) where
    type T   (DummyView f tx) = f tx
    type Key (DummyView f tx) = ()

    get = notImplemented
    set = notImplemented
    del = notImplemented

instance Ord tx => View (DummyView [] tx) where
    type Transaction (DummyView [] tx) = tx
    type BlockHeader (DummyView [] tx) = ()

    apply _ txs =
        for_ txs $ \tx ->
            State.modify (\txs -> sort $ tx : txs)

-- TestNode -------------------------------------------------------------------

data TestNode tx = TestNode (Addr (TestNode tx)) [tx] [Addr (TestNode tx)]
    deriving (Eq, Ord, Show)

instance (Ord tx, Eq tx) => Protocol (TestNode tx) where
    type Msg  (TestNode tx) = tx
    type Addr (TestNode tx) = Word8
    type Tick (TestNode tx) = NominalDiffTime

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

instance (Arbitrary tx, Ord tx) => TestableNode (TestNode tx) where
    type TestableTx (TestNode tx) = tx

    testableNode addr peers = TestNode addr [] peers
    testableNodeState (TestNode _ s _) = s
    isResting _ = True

-- BufferedTestNode -----------------------------------------------------------

data BufferedTestNode tx = BufferedTestNode
    { btnAddr    :: Addr (BufferedTestNode tx)
    , btnPeers   :: [Addr (BufferedTestNode tx)]
    , btnBuffer  :: [Msg (BufferedTestNode tx)]
    , btnTick    :: Tick (BufferedTestNode tx)
    , btnState   :: [tx]
    } deriving (Eq, Ord, Show)

instance Ord tx => Protocol (BufferedTestNode tx) where
    type Msg  (BufferedTestNode tx) = tx
    type Addr (BufferedTestNode tx) = Word8
    type Tick (BufferedTestNode tx) = NominalDiffTime

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

    epoch _ = 10

instance (Arbitrary tx, Ord tx) => TestableNode (BufferedTestNode tx) where
    type TestableTx (BufferedTestNode tx) = tx

    testableNode addr peers = BufferedTestNode
        { btnAddr   = addr
        , btnPeers  = peers
        , btnBuffer = []
        , btnTick   = 0
        , btnState  = []
        }
    testableNodeState = btnState
    isResting BufferedTestNode{btnBuffer} = null btnBuffer

-- Scheduled ------------------------------------------------------------------

data Scheduled a =
      ScheduledMessage (Tick a) (Addr a) (Addr a, Msg a)
    | ScheduledTick    (Tick a) (Addr a)

scheduledTick :: Scheduled a -> Tick a
scheduledTick (ScheduledMessage t _ _) = t
scheduledTick (ScheduledTick t _)      = t

instance Eq tx => Ord (Scheduled (TestNode tx)) where
    s <= s' = scheduledTick s <= scheduledTick s'

instance Eq tx => Ord (Scheduled (BufferedTestNode tx)) where
    s <= s' = scheduledTick s <= scheduledTick s'

deriving instance Eq tx   => Eq   (Scheduled (TestNode tx))
deriving instance Show tx => Show (Scheduled (TestNode tx))

deriving instance Eq   tx => Eq   (Scheduled (BufferedTestNode tx))
deriving instance Show tx => Show (Scheduled (BufferedTestNode tx))

-- TestNetwork ----------------------------------------------------------------

data TestNetwork a = TestNetwork
    { tnNodes :: Map (Addr a) a
    , tnMsgs  :: Set (Scheduled a)
    }

deriving instance Show tx => Show (TestNetwork (TestNode         tx))
deriving instance Show tx => Show (TestNetwork (BufferedTestNode tx))

instance TestableNode a => Arbitrary (TestNetwork a) where
    arbitrary = do
        addrs <- Set.fromList <$> vectorOf kidSize arbitrary :: Gen (Set (Addr a))

        nodes <- forM (toList addrs) $ \a ->
            pure (a, testableNode a [x | x <- toList addrs, x /= a])

        smsgs <- resize kidSize . listOf1 $ do
            msg <- arbitrary  :: Gen (Addr a, Msg a)
            dests <- sublistOf (toList addrs) :: Gen [Addr a]
            forM dests $ \d -> do
                at <- choose (0, 100) :: Gen Int
                pure $ ScheduledMessage (fromIntegral at) d msg

        ticks <- forM nodes $ \(addr, n) -> do
            pure [ScheduledTick (epoch n * fromIntegral x) addr | x <- [0..100] :: [Int]]

        pure $ TestNetwork
            { tnNodes = Map.fromList nodes
            , tnMsgs  = Set.fromList (concat $ smsgs ++ ticks)
            }

runNetwork :: TestableNode a => TestNetwork a -> TestNetwork a
runNetwork (TestNetwork nodes (Set.minView -> Just (ScheduledTick tick to, ms))) =
    runNetwork (scheduleMessages tick to msgs tn)
  where
    (tn, msgs) = deliverTick tick to (TestNetwork nodes ms)
runNetwork (TestNetwork nodes (Set.minView -> Just (ScheduledMessage tick to msg, ms))) =
    runNetwork (scheduleMessages tick to msgs tn)
  where
    (tn, msgs) = deliverMessage tick to msg (TestNetwork nodes ms)
runNetwork tn@(TestNetwork nodes ms)
    | Set.null ms && all isResting nodes = tn
    | otherwise                          = runNetwork tn

deliverTick
    :: (TestableNode a)
    => Tick a -> Addr a -> TestNetwork a -> (TestNetwork a, [(Addr a, Msg a)])
deliverTick tick to tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup to tnNodes =
        let (node', msgs) = step node tick Nothing
            nodes'        = Map.insert to node' tnNodes
         in (tn { tnNodes = nodes' }, msgs)
    | otherwise =
        (tn, [])

deliverMessage
    :: (TestableNode a)
    => Tick a -> Addr a -> (Addr a, Msg a) -> TestNetwork a -> (TestNetwork a, [(Addr a, Msg a)])
deliverMessage tick to (from, msg) tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup to tnNodes =
        let (node', msgs) = step node tick (Just (from, msg))
            nodes'        = Map.insert to node' tnNodes
         in (tn { tnNodes = nodes' }, msgs)
    | otherwise =
        (tn, [])

scheduleMessages
    :: (TestableNode a)
    => Tick a -> Addr a -> [(Addr a, Msg a)] -> TestNetwork a -> TestNetwork a
scheduleMessages t from msgs tn@TestNetwork{tnMsgs} =
    let deliveryTime  = t + 1
        scheduled     = Set.fromList [ScheduledMessage deliveryTime to (from, msg) | (to, msg) <- msgs]
        msgs'         = Set.union scheduled tnMsgs
     in tn { tnMsgs = msgs' }

networkHasMessages :: TestNetwork v -> Bool
networkHasMessages (TestNetwork _ ms)
    | Set.null ms = False
    | otherwise   = True

-------------------------------------------------------------------------------

mapMsgs :: Ord (Msg a) => Set (Scheduled a) -> Set (Msg a)
mapMsgs =
    foldr f Set.empty
  where
    f (ScheduledMessage _ _ (_, x)) acc = Set.insert x acc
    f   _                           acc = acc

propNetworkNodesIncludeAllTxns
    :: (Ord (Msg a), Ord (TestableTx a), TestableNode a)
    => TestNetwork a -> Property
propNetworkNodesIncludeAllTxns tn@(TestNetwork _nodes initialMsgs) =
    networkHasMessages tn ==>
        Set.size mappedInitialMsgs == Set.size (Set.fromList firstNodeMsgs)
            && Set.size nodeStates == 1
  where
    mappedInitialMsgs = mapMsgs initialMsgs
    TestNetwork nodes' _ = runNetwork tn
    firstNodeMsgs = testableNodeState $ head $ toList nodes'
    -- Deduplicating the different node states should result in a single state.
    nodeStates = Set.fromList $ map testableNodeState (toList nodes')
