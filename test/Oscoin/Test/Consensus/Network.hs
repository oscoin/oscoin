module Oscoin.Test.Consensus.Network
    ( TestableNode(..)
    , TestNetwork(..)
    , runNetwork
    , networkNonTrivial
    , testableLongestChain
    , testableNodeAddr
    , testableIncludedTxs
    , testableShow

    , Scheduled(..)
    , scheduledReceivers
    , scheduledTick
    , isMsg

    ) where

import           Oscoin.Prelude hiding (log, show)

import           Oscoin.Test.Consensus.Node

import           Oscoin.Crypto.Blockchain
                 (Blockchain, blocks, showChainDigest, unsafeToBlockchain)
import           Oscoin.Crypto.Blockchain.Block
                 (BlockHash, blockData, blockHash)
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Storage as Storage
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Storage.Block.Class
import           Oscoin.Storage.State.Class (MonadStateStore)
import           Oscoin.Time

import           Oscoin.Test.Consensus.Class

import           Codec.Serialise (Serialise)
import           Data.List (unlines)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Lens.Micro ((^.))
import           System.Random
import           System.Random.Shuffle (shuffle')
import           Text.Show (Show(..))

-- TestableNode ----------------------------------------------------------------

class
    ( MonadMempool DummyTx m
    , MonadBlockStore DummyTx s m
    , MonadStateStore DummyState m
    , HasTestNodeState s a
    ) => TestableNode s m a | a -> m, m -> a
  where
    testableInit         :: TestNetwork s b -> TestNetwork s a

    testableTick         :: Timestamp -> m [Msg DummyTx s]
    testableRun          :: a -> m b -> (b, a)
    testableScore        :: a -> Blockchain DummyTx s -> Int

testableLongestChain :: TestableNode s m a => a -> [BlockHash]
testableLongestChain =
      map blockHash
    . blocks
    . testableBestChain

testableNodeAddr :: TestableNode s m a => a -> DummyNodeId
testableNodeAddr nodeState = tnsNodeId $ nodeState ^. testNodeStateL

testableIncludedTxs :: TestableNode s m a => a -> [DummyTx]
testableIncludedTxs =
      concatMap (toList . blockData)
    . blocks
    . testableBestChain

testableShow :: TestableNode s m a => a -> Text
testableShow = showChainDigest . testableBestChain

testableBestChain :: TestableNode s m a => a -> Blockchain DummyTx s
testableBestChain nodeState =
    let blockStore = nodeState ^. testNodeStateL . tnsBlockstoreL
     in unsafeToBlockchain $ BlockStore.getBlocks 10000 blockStore
     -- XXX(alexis): Don't use a magic number.



-- TestNetwork -----------------------------------------------------------------

-- | The @TestNetwork s a@ models a set of nodes parameterized over the
-- node state @a@ and messages to be delivered between the nodes.
data TestNetwork s a = TestNetwork
    { tnNodes      :: Map DummyNodeId a
    , tnMsgs       :: Set (Scheduled s)
    -- ^ Events that are to be delivered to the network
    , tnPartitions :: Partitions
    -- ^ Determines which nodes can send messages between them
    , tnLog        :: [Scheduled s]
    -- ^ List of events the network has generated
    , tnLatencies  :: [Duration]
    -- ^ Infinite list of message latencies. When a node sends a
    -- message we take the first element to determine when the message
    -- gets delivered.
    , tnRng        :: StdGen
    , tnEval       :: DummyEval
    , tnMsgCount   :: Int
    -- ^ Number of messages delivered in the network
    , tnLastTick   :: Timestamp
    }

-- | Test evaluator.
type DummyEval = Evaluator DummyState DummyTx ()

-- | Network partitions is a map of node id to a set of node ids _not_
-- reachable from that node id.
type Partitions = Map DummyNodeId (Set DummyNodeId)

instance Serialise s => Show (TestNetwork s a) where
    show TestNetwork{..} =
        unlines [ "TestNetwork"
                , " nodes: "       ++ show (Map.keys tnNodes)
                , " last-tick: "   ++ show tnLastTick
                , " scheduled:\n"  ++ scheduled
                ]
      where
        scheduled = unlines
            ["  " ++ show msg | msg <- filter (not . isTick) (toList tnMsgs)]

-- | Takes the next scheduled event from 'tnMsgs' and applies it to the
-- network.
runNetwork :: (Ord s, Serialise s, TestableNode s m a) => TestNetwork s a -> TestNetwork s a
runNetwork tn@TestNetwork{tnMsgs, tnLastTick}
    | Just (nextMessage, remainingMessages) <- Set.minView tnMsgs
    , scheduledTick nextMessage <= tnLastTick =
        let tn' = tn { tnMsgs = remainingMessages }
        in runNetwork $ case nextMessage of
            ScheduledTick    tick to           -> deliver tick to Nothing    tn'
            ScheduledMessage tick to _from msg -> deliver tick to (Just msg) tn'
            Partition _ ps                     -> tn' { tnPartitions = ps     }
            Heal _                             -> tn' { tnPartitions = mempty }

    | otherwise = tn

-- | Advance the node with 'tickM' or 'stepM', then update the network
-- with the advanced node and schedule all messages generated by
-- advancing the node.
deliver
    :: (Ord s, TestableNode s m a)
    => Timestamp
    -> DummyNodeId
    -> Maybe (Msg DummyTx s)
    -> TestNetwork s a
    -> TestNetwork s a
deliver tick to msg tn@TestNetwork{tnNodes, tnMsgCount, tnEval}
    | Just node <- Map.lookup to tnNodes =
        let (outgoing, a) = testableRun node $ maybe (testableTick tick) (applyMessage tnEval) msg
            tnMsgCount'   = case msg of Just (BlockMsg _) -> tnMsgCount + 1;
                                                        _ -> tnMsgCount
            tn'           = tn { tnNodes    = Map.insert to a tnNodes
                               , tnMsgCount = tnMsgCount'
                               }
         in scheduleMessages tick to outgoing tn'

    | otherwise = tn

applyMessage :: (TestableNode s m a) => DummyEval -> Msg DummyTx s -> m [Msg DummyTx s]
applyMessage eval msg = go msg
  where
    go (TxMsg tx)     = resp <$> Storage.applyTx tx
    go (BlockMsg blk) = resp <$> Storage.applyBlock eval blk
    go (ReqBlockMsg blk) = do
        mblk <- Storage.lookupBlock blk
        pure . maybeToList . map BlockMsg $ mblk

    resp Storage.Applied = [msg]
    resp _               = []

scheduleMessages
    :: forall s a. Ord s
    => Timestamp
    -> DummyNodeId
    -> [Msg DummyTx s]
    -> TestNetwork s a
    -> TestNetwork s a
scheduleMessages t from msgs tn@TestNetwork{..} =
    tn { tnMsgs = msgs', tnLog = log, tnLatencies = tnLatencies', tnRng = tnRng' }
  where
    (lats, tnLatencies') = splitAt (length recipients) tnLatencies
    (rng, tnRng')        = split tnRng

    msgs' = Set.union (Set.fromList scheduled) tnMsgs
    log   = scheduled ++ tnLog

    -- Instead of sending the message to all peers, send it to at most 3 random peers.
    -- This ensures we're testing gossip functionality on larger networks.
    --
    -- Nb. Since currently this is very naive, we have pretty bad message amplification,
    -- because a node which receives a block might send that block back to the sender,
    -- since peer selection for gossip is completely random.
    --
    -- Ideally the P2P layer would keep track of who has what, so that we don't
    -- flood the network with redundant messages. Another way to solve this would be
    -- for the consensus protocol to be able to respond with a 'Gossip a' message which
    -- indicates that 'a' should not be sent to the sender, but only to other nodes.
    recipients = sample (peers tn from)
    sample [] = []
    sample xs = take 3 $ shuffle' xs (length xs) rng

    scheduled :: [Scheduled s]
    scheduled =
            (\(lat, (to, msg)) -> ScheduledMessage (t `timeAdd` lat) to from msg)
        <$> zip lats [(rcpt, msg) | rcpt <- recipients, msg <- msgs]

-- | Returns the list of nodes that are reachable from the given node
-- in the network taking into account the network partition.
peers :: TestNetwork s a -> DummyNodeId -> [DummyNodeId]
peers TestNetwork{..} node =
    let notReachable = fromMaybe Set.empty (Map.lookup node tnPartitions)
    in toList $ Set.delete node $ Map.keysSet tnNodes `Set.difference` notReachable


-- | A trivial network is a network without any nodes or scheduled
-- messages.
networkNonTrivial :: TestNetwork s v -> Bool
networkNonTrivial TestNetwork{tnNodes, tnMsgs}
    | Map.null tnNodes = False
    | Set.null msgs    = False
    | otherwise        = True
  where
    msgs = Set.filter isMsg tnMsgs

-- Scheduled -------------------------------------------------------------------

-- | An event that is to occur in a Node network at a specific tick.
data Scheduled s
    = ScheduledMessage Timestamp DummyNodeId DummyNodeId (Msg DummyTx s)
    -- ^ Message will be delivered from one node to another. First
    -- 'DummyNodeId' is the receipient, second the sender. Results in
    -- calling 'stepM' on the receipient.
    | ScheduledTick    Timestamp DummyNodeId
    -- ^ The given node will execute its 'tickM' function
    | Partition        Timestamp Partitions
    -- ^ Set the partition of the network
    | Heal             Timestamp
    -- ^ Remove any partitioning of the network
    deriving (Eq, Show)

instance Ord s => Ord (Scheduled s) where
    s <= s' = scheduledTick s <= scheduledTick s'

scheduledTick :: Scheduled s -> Timestamp
scheduledTick (ScheduledMessage t _ _ _) = t
scheduledTick (ScheduledTick t _)        = t
scheduledTick (Partition t _)            = t
scheduledTick (Heal t)                   = t

scheduledReceivers :: Scheduled s -> [DummyNodeId]
scheduledReceivers (ScheduledMessage _ a _ _) = [a]
scheduledReceivers (ScheduledTick _ a)        = [a]
scheduledReceivers (Partition _ _)            = [] -- XXX
scheduledReceivers (Heal _)                   = []

isTick :: Scheduled s -> Bool
isTick (ScheduledTick _ _) = True
isTick _                   = False

isMsg :: Scheduled s -> Bool
isMsg ScheduledMessage{} = True
isMsg _                  = False
