module Oscoin.Test.Consensus.Network
    ( TestableNode(..)
    , TestNetwork(..)
    , runNetwork
    , networkNonTrivial
    , testableLongestChain
    , testableNodeAddr
    , testableIncludedTxs
    , testableShow

    , Msg(TxMsg)

    , Scheduled(..)
    , scheduledReceiver
    , scheduledTick
    , prettyScheduled
    , isMsg
    , isTick

    ) where

import           Oscoin.Prelude hiding (log, show)

import           Oscoin.Test.Consensus.Node

import           Oscoin.Consensus (Validate)
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain
                 ( Blockchain
                 , blocks
                 , showBlockDigest
                 , showChainDigest
                 , unsafeToBlockchain
                 )
import           Oscoin.Crypto.Blockchain.Block
                 ( Block
                 , BlockHash
                 , blockData
                 , blockHash
                 , blockHeader
                 , blockPrevHash
                 )
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Storage as Storage
import           Oscoin.Storage.Block.Class
import qualified Oscoin.Storage.Block.Pure as BlockStore
import           Oscoin.Storage.State.Class (MonadStateStore)
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Data.List (unlines)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Formatting (left, right, sformat, shown, stext, (%), (%.))
import qualified Formatting as F
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

    testableTick         :: Timestamp -> m (Maybe (Block DummyTx s))
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

-- Msg ------------------------------------------------------------------------

data Msg s =
      BlockMsg    (Block DummyTx s)
    -- ^ Message containing a 'Block'.
    | TxMsg       DummyTx
    -- ^ Message containing a @tx@.
    | ReqBlockMsg DummyNodeId BlockHash
    -- ^ Message requesting a 'Block' with a certain hash. 'DummyNodeId' is the requesting node.
    | ResBlockMsg DummyNodeId (Block DummyTx s)
    -- ^ Message sent in response to 'ReqBlockMsg'. 'DummyNodeId' is the requesting node.
    deriving (Eq)

instance Show (Msg s) where
    show (BlockMsg  blk)   = F.formatToString (F.stext % " " % F.stext) "BlockMsg" (showBlockDigest blk)
    show (TxMsg     txs)   = "TxMsg " ++ show txs
    show (ReqBlockMsg n h) = "ReqBlockMsg " ++ "(" ++ show n ++ ") " ++ show (Crypto.shortHash h)
    show (ResBlockMsg n b) = "ResBlockMsg " ++ "(" ++ show n ++ ") " ++ T.unpack (showBlockDigest b)

-- TestNetwork -----------------------------------------------------------------

-- | The @TestNetwork s a@ models a set of nodes parameterized over the
-- node state @a@ and messages to be delivered between the nodes.
data TestNetwork s a = TestNetwork
    { tnNodes      :: Map DummyNodeId a
    , tnScheduled  :: Set (Scheduled s)
    -- ^ Initial set of scheduled events.
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
    , tnValidate   :: DummyValidate s
    , tnMsgCount   :: Int
    -- ^ Number of messages delivered in the network
    , tnLastTick   :: Timestamp
    }

-- | Test evaluator.
type DummyEval = Evaluator DummyState DummyTx ()

-- | Test block validator.
type DummyValidate s = Validate DummyTx s

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
            ["  " ++ T.unpack (prettyScheduled msg) | msg <- filter (not . isTick) (toList tnScheduled)]

prettyPartitions :: Partitions -> Text
prettyPartitions parts =
    T.pack $ show $ Map.toList parts

-- | Run the network simulation.
runNetwork :: (Ord s, Serialise s, TestableNode s m a)
           => Consensus.Config
           -> TestNetwork s a
           -> TestNetwork s a
runNetwork config tn@TestNetwork{tnScheduled} =
    runNetwork' config tn { tnMsgs = tnScheduled }

-- | Takes the next scheduled event from 'tnMsgs' and applies it to the
-- network.
runNetwork' :: (Ord s, Serialise s, TestableNode s m a)
            => Consensus.Config
            -> TestNetwork s a
            -> TestNetwork s a
runNetwork' config tn@TestNetwork{tnMsgs, tnLastTick}
    | Just (nextMessage, remainingMessages) <- Set.minView tnMsgs
    , scheduledTick nextMessage <= tnLastTick =
        let tn' = tn { tnMsgs = remainingMessages }
        in runNetwork' config $ case nextMessage of
            ScheduledTick    tick to           -> deliver config tick to Nothing    tn'
            ScheduledMessage tick to _from msg -> deliver config tick to (Just msg) tn'
            Partition _ ps                     -> tn' { tnPartitions = ps     }
            Heal _                             -> tn' { tnPartitions = mempty }

    | otherwise = tn

-- | Deliver a message to the given node and update the network. The delivery
-- may trigger new outgoing messages to be sent.
deliver
    :: (Ord s, Serialise s, TestableNode s m a)
    => Consensus.Config
    -> Timestamp
    -> DummyNodeId
    -> Maybe (Msg s)
    -> TestNetwork s a
    -> TestNetwork s a
deliver config tick to msg tn@TestNetwork{tnNodes, tnMsgCount, tnValidate, tnEval}
    | Just node <- Map.lookup to tnNodes =
        let (outgoing, a) = testableRun node $ maybe (applyTick tick) (applyMessage config to tnValidate tnEval) msg
            tnMsgCount'   = case msg of Just (BlockMsg _) -> tnMsgCount + 1;
                                                        _ -> tnMsgCount
            tn'           = tn { tnNodes    = Map.insert to a tnNodes
                               , tnMsgCount = tnMsgCount'
                               }
         in scheduleMessages tick to outgoing tn'

    | otherwise = tn

applyTick :: TestableNode s m a => Timestamp -> m [Msg s]
applyTick t =
    map BlockMsg . maybeToList <$> testableTick t

-- | Apply a message to the node state. Returns a list of messages to send back
-- to the network.
applyMessage :: (TestableNode s m a, Serialise s)
             => Consensus.Config
             -> DummyNodeId
             -> DummyValidate s
             -> DummyEval
             -> Msg s
             -> m [Msg s]
applyMessage _ _ _ _ msg@(TxMsg tx) = do
    result <- Storage.applyTx tx
    pure $ case result of
        Storage.Applied _ -> [msg]
        _                 -> []
applyMessage config to validate eval msg@(BlockMsg blk) = do
    result          <- Storage.applyBlock eval validate config blk
    parentMissing   <- isNothing <$> Storage.lookupBlock parentHash

    -- If the parent block is missing, request it from the network.
    pure $ case result of
        Storage.Applied _
            | parentMissing -> [msg, ReqBlockMsg to parentHash]
            | otherwise     -> [msg]
        _ -> []
  where
    parentHash = blockPrevHash $ blockHeader blk
applyMessage _ _ _ _ (ReqBlockMsg from bh) = do
    mblk <- Storage.lookupBlock bh
    pure . maybeToList . map (ResBlockMsg from) $ mblk
applyMessage config to validate eval (ResBlockMsg _ blk) =
    applyMessage config to validate eval (BlockMsg blk)

-- | Schedules a list of messages to be delivered by the simulation to a set
-- of random nodes at some point in the future.
scheduleMessages
    :: forall s a. Ord s
    => Timestamp
    -> DummyNodeId
    -> [Msg s]
    -> TestNetwork s a
    -> TestNetwork s a
scheduleMessages t from msgs tn@TestNetwork{..} =
    tn { tnMsgs = msgs', tnLog = log, tnLatencies = tnLatencies', tnRng = tnRng' }
  where
    (lats, tnLatencies') = splitAt (length messages) tnLatencies
    (rng, tnRng')        = split tnRng

    msgs' = Set.union (Set.fromList $ scheduled messages) tnMsgs
    log   = scheduled messages ++ tnLog

    messages :: [(Msg s, DummyNodeId)]
    messages = concat [[(msg, r) | r <- recipients msg] | msg <- msgs]

    recipients :: Msg s -> [DummyNodeId]
    recipients (ResBlockMsg id _) = [id]
    recipients _                  = sampledPeers 3

    scheduled :: [(Msg s, DummyNodeId)] -> [Scheduled s]
    scheduled [] = []
    scheduled xs =
        [ScheduledMessage (t `timeAdd` lat `timeAdd` (1 * nanoseconds)) to from msg
            | (lat, (msg, to)) <- zip lats xs]

    sampledPeers n = sample n (peers tn from)

    sample _ [] = []
    sample n xs = take n $ shuffle' xs (length xs) rng

-- | Returns the list of nodes that are reachable from the given node
-- in the network taking into account the network partition.
peers :: TestNetwork s a -> DummyNodeId -> [DummyNodeId]
peers TestNetwork{..} node =
    let notReachable = fromMaybe Set.empty (Map.lookup node tnPartitions)
    in toList $ Set.delete node $ Map.keysSet tnNodes `Set.difference` notReachable


-- | A trivial network is a network without any nodes or scheduled
-- messages.
networkNonTrivial :: TestNetwork s v -> Bool
networkNonTrivial TestNetwork{tnNodes, tnScheduled}
    | Map.null tnNodes = False
    | Set.null msgs    = False
    | otherwise        = True
  where
    msgs = Set.filter isMsg tnScheduled

-- Scheduled -------------------------------------------------------------------

-- | An event that is to occur in a Node network at a specific tick.
data Scheduled s
    = ScheduledMessage Timestamp DummyNodeId DummyNodeId (Msg s)
    -- ^ Message will be delivered from one node to another. First
    -- 'DummyNodeId' is the recipient, second the sender. Results in
    -- calling 'stepM' on the recipient.
    | ScheduledTick    Timestamp DummyNodeId
    -- ^ The given node will execute its 'tickM' function
    | Partition        Timestamp Partitions
    -- ^ Set the partition of the network
    | Heal             Timestamp
    -- ^ Remove any partitioning of the network
    deriving (Eq, Show)

prettyScheduled :: Scheduled s -> Text
prettyScheduled (ScheduledMessage t to from msg) =
    sformat ( left 5 ' '
            % " Message: "
            % (right 3 ' ' %. shown)
            % " -> "
            % (right 3 ' ' %. shown)
            % ": " % shown
            ) (prettyDuration (sinceEpoch t)) from to msg
prettyScheduled (ScheduledTick t node) =
    sformat ( left 5 ' '
            % " Tick: "
            % shown
            ) (prettyDuration (sinceEpoch t)) node
prettyScheduled (Partition t parts) =
    sformat ( left 5 ' '
            % " Partition: "
            % stext
            ) (prettyDuration (sinceEpoch t))
              (prettyPartitions parts)
prettyScheduled (Heal t) =
    sformat ( left 5 ' '
            % " Heal"
            ) (prettyDuration (sinceEpoch t))

instance Ord s => Ord (Scheduled s) where
    s <= s' = scheduledTick s <= scheduledTick s'

scheduledTick :: Scheduled s -> Timestamp
scheduledTick (ScheduledMessage t _ _ _) = t
scheduledTick (ScheduledTick t _)        = t
scheduledTick (Partition t _)            = t
scheduledTick (Heal t)                   = t

scheduledReceiver :: Scheduled s -> Maybe DummyNodeId
scheduledReceiver (ScheduledMessage _ a _ _) = Just a
scheduledReceiver (ScheduledTick _ a)        = Just a
scheduledReceiver (Partition _ _)            = Nothing
scheduledReceiver (Heal _)                   = Nothing

isTick :: Scheduled s -> Bool
isTick (ScheduledTick _ _) = True
isTick _                   = False

isMsg :: Scheduled s -> Bool
isMsg ScheduledMessage{} = True
isMsg _                  = False
