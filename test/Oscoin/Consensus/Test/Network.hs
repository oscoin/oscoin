module Oscoin.Consensus.Test.Network where

import           Oscoin.Prelude
import           Oscoin.Consensus.Test.Node
import           Oscoin.Consensus.Class

import qualified Data.Set as Set
import qualified Data.Map as Map

import           Test.QuickCheck

-- TestableNode ---------------------------------------------------------------

class ( Eq (TestableTx a)
      , Eq a
      , Show (TestableTx a)
      , Ord (Scheduled a)
      , Ord (Addr a)
      , Arbitrary (Msg a)
      , Arbitrary (Addr a)
      , Protocol a ) => TestableNode a where
    type TestableTx a :: *

    testableNode :: Addr a -> [Addr a] -> a
    testableNodeState :: a -> [TestableTx a]

instance (Show tx, Arbitrary tx, Ord tx) => TestableNode (TestNode tx) where
    type TestableTx (TestNode tx) = tx

    testableNode addr peers = TestNode addr [] peers
    testableNodeState (TestNode _ s _) = s

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

-- TestNetwork ----------------------------------------------------------------

data TestNetwork a = TestNetwork
    { tnNodes      :: Map (Addr a) a
    , tnMsgs       :: Set (Scheduled a)
    , tnPartitions :: Map (Addr a) (Set (Addr a))
    }

deriving instance Show tx => Show (TestNetwork (TestNode         tx))
deriving instance Show tx => Show (TestNetwork (BufferedTestNode tx))

runNetwork :: TestableNode a => TestNetwork a -> TestNetwork a
runNetwork (TestNetwork nodes (Set.minView -> Just (ScheduledTick tick to, ms)) partitions) =
    runNetwork (scheduleMessages tick to msgs tn)
  where
    (tn, msgs) = deliverTick tick to (TestNetwork nodes ms partitions)
runNetwork (TestNetwork nodes (Set.minView -> Just (ScheduledMessage tick to msg, ms)) partitions)  =
    runNetwork (scheduleMessages tick to msgs tn)
  where
    (tn, msgs) = deliverMessage tick to msg (TestNetwork nodes ms partitions)
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
        reachable to = maybe True not $ do
            blacklist <- Map.lookup from tnPartitions
            pure $ Set.member to blacklist
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
    | Disconnect       (Tick a) (Addr a) (Addr a)
    | Reconnect        (Tick a) (Addr a) (Addr a)

scheduledTick :: Scheduled a -> Tick a
scheduledTick (ScheduledMessage t _ _) = t
scheduledTick (ScheduledTick t _)      = t
scheduledTick (Disconnect t _ _)       = t
scheduledTick (Reconnect t _ _)        = t

scheduledReceivers :: Scheduled a -> [Addr a]
scheduledReceivers (ScheduledMessage _ a _) = [a]
scheduledReceivers (ScheduledTick _ a)      = [a]
scheduledReceivers (Disconnect _ f t)       = [t, f]
scheduledReceivers (Reconnect _ f t)        = [t, f]

scheduledSender :: Scheduled a -> Maybe (Addr a)
scheduledSender (ScheduledMessage _ _ (a, _)) = Just a
scheduledSender _                             = Nothing

scheduledMessage :: Scheduled a -> Maybe (Msg a)
scheduledMessage (ScheduledMessage _ _ (_, x)) = Just x
scheduledMessage _                             = Nothing

instance Eq tx => Ord (Scheduled (TestNode tx)) where
    s <= s' = scheduledTick s <= scheduledTick s'

instance Eq tx => Ord (Scheduled (BufferedTestNode tx)) where
    s <= s' = scheduledTick s <= scheduledTick s'

deriving instance Eq tx   => Eq   (Scheduled (TestNode tx))
deriving instance Show tx => Show (Scheduled (TestNode tx))

deriving instance Eq   tx => Eq   (Scheduled (BufferedTestNode tx))
deriving instance Show tx => Show (Scheduled (BufferedTestNode tx))

