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
import           Data.List (sort)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (NominalDiffTime)

tests :: [TestTree]
tests = [ testProperty "All nodes include all txns"            (propNetworkNodesIncludeAllTxns @(TestNode DummyState))
        , testProperty "All nodes include all txns (buffered)" (propNetworkNodesIncludeAllTxns @(BufferedTestNode DummyState)) ]

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 11

-- TNode ----------------------------------------------------------------------

class ( Eq (TNodeTx a)
      , Ord (Scheduled a)
      , Ord (Addr a)
      , Arbitrary (Msg a)
      , Arbitrary (Addr a)
      , Protocol a) => TNode a where
    type TNodeTx a :: *

    nodeState :: a -> [TNodeTx a]
    isResting :: a -> Bool
    newNode   :: Addr a -> [Addr a] -> a

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
    type Addr (TestNode DummyState) = Word8
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
    newNode addr peers = TestNode addr [] peers

-- BufferedTestNode -----------------------------------------------------------

data BufferedTestNode s = BufferedTestNode
    { btnAddr    :: Addr (BufferedTestNode s)
    , btnPeers   :: [Addr (BufferedTestNode s)]
    , btnBuffer  :: [Msg (BufferedTestNode s)]
    , btnTick    :: Tick (BufferedTestNode s)
    , btnState   :: s
    }

deriving instance Eq (BufferedTestNode DummyState)
deriving instance Ord (BufferedTestNode DummyState)
deriving instance Show (BufferedTestNode DummyState)

instance Protocol (BufferedTestNode DummyState) where
    type Msg  (BufferedTestNode DummyState) = DummyTx
    type Addr (BufferedTestNode DummyState) = Word8
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

    epoch _ = 10

instance TNode (BufferedTestNode DummyState) where
    type TNodeTx (BufferedTestNode DummyState) = DummyTx

    nodeState = btnState
    isResting BufferedTestNode{btnBuffer} = null btnBuffer
    newNode addr peers = BufferedTestNode
        { btnAddr   = addr
        , btnPeers  = peers
        , btnBuffer = []
        , btnTick   = 0
        , btnState  = []
        }

-- Scheduled ------------------------------------------------------------------

data Scheduled a =
      ScheduledMessage (Tick a) (Addr a) (Addr a, Msg a)
    | ScheduledTick    (Tick a) (Addr a)

scheduledTick :: Scheduled a -> Tick a
scheduledTick (ScheduledMessage t _ _) = t
scheduledTick (ScheduledTick t _)      = t

instance Ord (Scheduled (TestNode DummyState)) where
    s <= s' = scheduledTick s <= scheduledTick s'

instance Ord (Scheduled (BufferedTestNode DummyState)) where
    s <= s' = scheduledTick s <= scheduledTick s'

deriving instance Eq   (Scheduled (TestNode DummyState))
deriving instance Show (Scheduled (TestNode DummyState))

deriving instance Eq   (Scheduled (BufferedTestNode DummyState))
deriving instance Show (Scheduled (BufferedTestNode DummyState))

-- TestNetwork ----------------------------------------------------------------

-- TODO(tyler): Better data structure for scheduled messages.
data TestNetwork a = TestNetwork
    { tnNodes :: Map (Addr a) a
    , tnMsgs  :: Set (Scheduled a)
    }

deriving instance Show (TestNetwork (TestNode         DummyState))
deriving instance Show (TestNetwork (BufferedTestNode DummyState))

instance TNode a => Arbitrary (TestNetwork a) where
    arbitrary = do
        addrs <- Set.fromList <$> vectorOf kidSize arbitrary :: Gen (Set (Addr a))

        nodes <- forM (toList addrs) $ \a ->
            pure (a, newNode a [x | x <- toList addrs, x /= a])

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
            , tnMsgs  = Set.fromList (mconcat $ smsgs ++ ticks)
            }

runNetwork :: TNode a => TestNetwork a -> TestNetwork a
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
    :: (TNode a)
    => Tick a -> Addr a -> TestNetwork a -> (TestNetwork a, [(Addr a, Msg a)])
deliverTick tick to tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup to tnNodes =
        let (node', msgs) = step node tick Nothing
            nodes'        = Map.insert to node' tnNodes
         in (tn { tnNodes = nodes' }, msgs)
    | otherwise =
        (tn, [])

deliverMessage
    :: (TNode a)
    => Tick a -> Addr a -> (Addr a, Msg a) -> TestNetwork a -> (TestNetwork a, [(Addr a, Msg a)])
deliverMessage tick to (from, msg) tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup to tnNodes =
        let (node', msgs) = step node tick (Just (from, msg))
            nodes'        = Map.insert to node' tnNodes
         in (tn { tnNodes = nodes' }, msgs)
    | otherwise =
        (tn, [])

scheduleMessages
    :: (TNode a)
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
    :: (Ord (Msg a), Ord (TNodeTx a), TNode a)
    => TestNetwork a -> Property
propNetworkNodesIncludeAllTxns tn@(TestNetwork _nodes initialMsgs) =
    networkHasMessages tn ==>
        Set.size mappedInitialMsgs == Set.size (Set.fromList firstNodeMsgs)
            && Set.size nodeStates == 1
  where
    mappedInitialMsgs = mapMsgs initialMsgs
    TestNetwork nodes' _ = runNetwork tn
    firstNodeMsgs = nodeState $ head $ toList nodes'
    -- Deduplicating the different node states should result in a single state.
    nodeStates = Set.fromList $ map nodeState (toList nodes')
