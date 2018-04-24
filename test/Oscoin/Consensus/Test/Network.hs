module Oscoin.Consensus.Test.Network where

import           Oscoin.Prelude
import           Oscoin.Consensus.Test.Node
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Simple

import qualified Data.Set as Set
import qualified Data.Map as Map

import           Test.QuickCheck

-- TestableNode ---------------------------------------------------------------

class ( Eq (TestableTx a)
      , Eq a
      , Show (TestableTx a)
      , Ord (Scheduled a)
      , Show (Scheduled a)
      , Ord (Addr a)
      , Show (Addr a)
      , Arbitrary (Msg a)
      , Arbitrary (Addr a)
      , Eq (Msg a)
      , Show (Tick a)
      , Show (Msg a)
      , Protocol a ) => TestableNode a where
    type TestableTx a :: *

    testableNode :: Addr a -> [Addr a] -> a
    testableNodeState :: a -> [TestableTx a]
    testableNodeAddr :: a -> Addr a

instance (Show tx, Arbitrary tx, Ord tx) => TestableNode (TestNode tx) where
    type TestableTx (TestNode tx) = tx

    testableNode addr = TestNode addr []
    testableNodeState (TestNode _ s _) = s
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
    testableNodeState = btnState
    testableNodeAddr = btnAddr

instance (Show tx, Arbitrary tx, Ord tx) => TestableNode (SimpleNode tx) where
    type TestableTx (SimpleNode tx) = tx

    testableNode addr peers = SimpleNode
        { snAddr   = addr
        , snPeers  = peers
        , snBuffer = []
        , snTick   = 0
        , snState  = []
        }
    testableNodeState = snState
    testableNodeAddr = snAddr

-- TestNetwork ----------------------------------------------------------------

data TestNetwork a = TestNetwork
    { tnNodes      :: Map (Addr a) a
    , tnMsgs       :: Set (Scheduled a)
    , tnPartitions :: Partitions a
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
runNetwork (TestNetwork nodes (Set.minView -> Just (ScheduledTick tick to, ms)) partitions) =
    runNetwork (scheduleMessages tick to msgs tn)
  where
    (tn, msgs) = deliverTick tick to (TestNetwork nodes ms partitions)
runNetwork (TestNetwork nodes (Set.minView -> Just (ScheduledMessage tick to msg, ms)) partitions)  =
    runNetwork (scheduleMessages tick to msgs tn)
  where
    (tn, msgs) = deliverMessage tick to msg (TestNetwork nodes ms partitions)
runNetwork (TestNetwork nodes (Set.minView -> Just (Partition _ partitions, ms)) _)  =
    runNetwork (TestNetwork nodes ms partitions)
runNetwork (TestNetwork nodes (Set.minView -> Just (Heal _, ms)) _)  =
    runNetwork (TestNetwork nodes ms mempty)
runNetwork (TestNetwork nodes (Set.minView -> Just (Disconnect _ from to, ms)) partitions)  =
    runNetwork (TestNetwork nodes ms newPartitions)
  where
    newPartitions             = Map.alter alteration from partitions
    alteration (Just current) = Just (Set.insert to current)
    alteration Nothing        = Just (Set.singleton to)
runNetwork (TestNetwork nodes (Set.minView -> Just (Reconnect _ from to, ms)) partitions)  =
    runNetwork (TestNetwork nodes ms newPartitions)
  where
    alteration    = map (Set.delete to)
    newPartitions = Map.alter alteration from partitions
runNetwork tn@(TestNetwork _ ms _)
    | Set.null ms = tn
    | otherwise   = runNetwork tn

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
scheduleMessages t from msgs tn@TestNetwork{tnMsgs, tnPartitions} =
    let deliveryTime = t + 1
        msgs'        = Set.union scheduled tnMsgs
        scheduled    = Set.fromList [ScheduledMessage deliveryTime to (from, msg) | (to, msg) <- msgs, reachable to]
        reachable to = maybe True not $
            Set.member to <$> Map.lookup from tnPartitions
     in tn { tnMsgs = msgs' }

networkNonTrivial :: TestNetwork v -> Bool
networkNonTrivial (TestNetwork ns ms _)
    | Map.null ns = False
    | Set.null ms = False
    | otherwise   = True

-- Scheduled ------------------------------------------------------------------

data Scheduled a =
      ScheduledMessage (Tick a) (Addr a) (Addr a, Msg a)
    | ScheduledTick    (Tick a) (Addr a)
    | Partition        (Tick a) (Partitions a)
    | Heal             (Tick a)
    | Disconnect       (Tick a) (Addr a) (Addr a)
    | Reconnect        (Tick a) (Addr a) (Addr a)

deriving instance TestableNode a => Show (Scheduled a)
deriving instance TestableNode a => Eq (Scheduled a)

instance TestableNode a => Ord (Scheduled a) where
    s <= s' = scheduledTick s <= scheduledTick s'

scheduledTick :: Scheduled a -> Tick a
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
isMsg (ScheduledMessage _ _ _) = True
isMsg _                        = False
