module Oscoin.Consensus.Test.Network where

import           Oscoin.Prelude
import           Oscoin.Consensus.Test.Node
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Simple
import           Oscoin.Consensus.Simple.Arbitrary ()

import           Data.Binary (Binary)
import qualified Data.Set as Set
import qualified Data.Map as Map

import           Test.QuickCheck

-- TestableNode ---------------------------------------------------------------

class ( Eq (TestableTx a)
      , Show (TestableTx a)
      , Arbitrary (Msg a)
      , Arbitrary (Addr a)
      , Eq (Msg a)
      , Show (Scheduled a)
      , Show (Addr a)
      , Show (Msg a)
      , Protocol a ) => TestableNode a where
    type TestableTx a :: *

    testableNode :: Addr a -> [Addr a] -> a
    testablePostState :: a -> [TestableTx a]
    testablePreState :: a -> Msg a -> [TestableTx a]
    testableNodeAddr :: a -> Addr a


instance (Show tx, Arbitrary tx, Ord tx) => TestableNode (TestNode tx) where
    type TestableTx (TestNode tx) = tx

    testableNode addr = TestNode addr []
    testablePreState _ tx = [tx]
    testablePostState (TestNode _ s _) = s
    testableNodeAddr (TestNode a _ _) = a

instance (Show tx, Arbitrary tx, Ord tx) => TestableNode (BufferedTestNode tx) where
    type TestableTx (BufferedTestNode tx) = tx

    testableNode addr peers = BufferedTestNode
        { btnAddr   = addr
        , btnPeers  = peers
        , btnBuffer = []
        , btnTick   = 0
        , btnState  = []
        }
    testablePreState _ tx = [tx]
    testablePostState = btnState
    testableNodeAddr = btnAddr

instance (Binary tx, Show tx, Arbitrary tx, Ord tx) => TestableNode (SimpleNode tx) where
    type TestableTx (SimpleNode tx) = tx

    testableNode addr peers = SimpleNode
        { snAddr   = addr
        , snPeers  = peers
        , snBuffer = mempty
        , snTick   = 0
        , snStore  = genesisBlockStore
        }

    testablePreState _ (ClientTx tx) = [tx]
    testablePreState _ _             = []

    testablePostState :: SimpleNode tx -> [TestableTx (SimpleNode tx)]
    testablePostState node = chainTxs $ bestChain $ snStore node

    testableNodeAddr = snAddr

-- TestNetwork ----------------------------------------------------------------

data TestNetwork a = TestNetwork
    { tnNodes      :: Map (Addr a) a
    , tnMsgs       :: Set (Scheduled a)
    , tnPartitions :: Partitions a
    , tnLog        :: [Scheduled a]
    }

type Partitions a = Map (Addr a) (Set (Addr a))

instance (TestableNode a, Show a) => Show (TestNetwork a) where
    show TestNetwork{..} =
        unlines [ "TestNetwork"
                , " nodes: "      ++ show (map testableNodeAddr (toList tnNodes))
                , " scheduled:\n" ++ scheduled
                ]
      where
        scheduled = unlines
            ["  " ++ show msg | msg <- filter (not . isTick) (toList tnMsgs)]

runNetwork :: TestableNode a => TestNetwork a -> TestNetwork a
runNetwork (TestNetwork nodes (Set.minView -> Just (ScheduledTick tick to, ms)) partitions log) =
    runNetwork (scheduleMessages tick to msgs tn)
  where
    (tn, msgs) = deliverTick tick to (TestNetwork nodes ms partitions log)
runNetwork (TestNetwork nodes (Set.minView -> Just (ScheduledMessage tick to msg, ms)) partitions log)  =
    runNetwork (scheduleMessages tick to msgs tn)
  where
    (tn, msgs) = deliverMessage tick to msg (TestNetwork nodes ms partitions log)
runNetwork (TestNetwork nodes (Set.minView -> Just (Partition _ partitions, ms)) _ log)  =
    runNetwork (TestNetwork nodes ms partitions log)
runNetwork (TestNetwork nodes (Set.minView -> Just (Heal _, ms)) _ log)  =
    runNetwork (TestNetwork nodes ms mempty log)
runNetwork (TestNetwork nodes (Set.minView -> Just (Disconnect _ from to, ms)) partitions log)  =
    runNetwork (TestNetwork nodes ms newPartitions log)
  where
    newPartitions             = Map.alter alteration from partitions
    alteration (Just current) = Just (Set.insert to current)
    alteration Nothing        = Just (Set.singleton to)
runNetwork (TestNetwork nodes (Set.minView -> Just (Reconnect _ from to, ms)) partitions log)  =
    runNetwork (TestNetwork nodes ms newPartitions log)
  where
    alteration    = map (Set.delete to)
    newPartitions = Map.alter alteration from partitions
runNetwork tn@(TestNetwork _ ms _ _)
    | Set.null ms = tn
    | otherwise   = runNetwork tn

deliverTick
    :: (TestableNode a)
    => Tick -> Addr a -> TestNetwork a -> (TestNetwork a, [(Addr a, Msg a)])
deliverTick tick to tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup to tnNodes =
        let (node', msgs) = step node tick Nothing
            nodes'        = Map.insert to node' tnNodes
         in (tn { tnNodes = nodes' }, msgs)
    | otherwise =
        (tn, [])

deliverMessage
    :: (TestableNode a)
    => Tick -> Addr a -> (Addr a, Msg a) -> TestNetwork a -> (TestNetwork a, [(Addr a, Msg a)])
deliverMessage tick to (from, msg) tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup to tnNodes =
        let (node', msgs) = step node tick (Just (from, msg))
            nodes'        = Map.insert to node' tnNodes
         in (tn { tnNodes = nodes' }, msgs)
    | otherwise =
        (tn, [])

scheduleMessages
    :: (TestableNode a)
    => Tick -> Addr a -> [(Addr a, Msg a)] -> TestNetwork a -> TestNetwork a
scheduleMessages t from msgs tn@TestNetwork{tnMsgs, tnPartitions, tnLog} =
    let deliveryTime = t + 1
        msgs'        = Set.union (Set.fromList scheduled) tnMsgs
        log          = scheduled ++ tnLog
        scheduled    = [ScheduledMessage deliveryTime to (from, msg) | (to, msg) <- msgs, reachable to]
        reachable to = maybe True not $
            Set.member to <$> Map.lookup from tnPartitions
     in tn { tnMsgs = msgs', tnLog = log }

networkNonTrivial :: TestNetwork v -> Bool
networkNonTrivial (TestNetwork ns ms _ _)
    | Map.null ns = False
    | Set.null ms = False
    | otherwise   = True

-- Scheduled ------------------------------------------------------------------

data Scheduled a =
      ScheduledMessage Tick (Addr a) (Addr a, Msg a)
    | ScheduledTick    Tick (Addr a)
    | Partition        Tick (Partitions a)
    | Heal             Tick
    | Disconnect       Tick (Addr a) (Addr a)
    | Reconnect        Tick (Addr a) (Addr a)

deriving instance TestableNode a => Show (Scheduled a)
deriving instance TestableNode a => Eq (Scheduled a)

instance TestableNode a => Ord (Scheduled a) where
    s <= s' = scheduledTick s <= scheduledTick s'

scheduledTick :: Scheduled a -> Tick
scheduledTick (ScheduledMessage t _ _) = t
scheduledTick (ScheduledTick t _)      = t
scheduledTick (Partition t _)          = t
scheduledTick (Heal t)                 = t
scheduledTick (Disconnect t _ _)       = t
scheduledTick (Reconnect t _ _)        = t

scheduledReceivers :: Scheduled a -> [Addr a]
scheduledReceivers (ScheduledMessage _ a _) = [a]
scheduledReceivers (ScheduledTick _ a)      = [a]
scheduledReceivers (Partition _ _)          = [] -- XXX
scheduledReceivers (Heal _)                 = []
scheduledReceivers (Disconnect _ f t)       = [t, f]
scheduledReceivers (Reconnect _ f t)        = [t, f]

scheduledSender :: Scheduled a -> Maybe (Addr a)
scheduledSender (ScheduledMessage _ _ (a, _)) = Just a
scheduledSender _                             = Nothing

scheduledMessage :: Scheduled a -> Maybe (Msg a)
scheduledMessage (ScheduledMessage _ _ (_, x)) = Just x
scheduledMessage _                             = Nothing

isTick :: Scheduled a -> Bool
isTick (ScheduledTick _ _) = True
isTick _                   = False

isMsg :: Scheduled a -> Bool
isMsg ScheduledMessage{} = True
isMsg _                  = False
