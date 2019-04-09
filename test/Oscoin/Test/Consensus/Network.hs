{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Test.Consensus.Network
    ( TestableNode(..)
    , TestNetwork(..)
    , runNetwork
    , networkNonTrivial
    , initNetwork
    , testableLongestChain
    , testableIncludedTxs
    , testableShow

    , DummyNodeId(..)

    , Msg(TxMsg)

    , Scheduled(..)
    , scheduledReceiver
    , scheduledTick
    , prettyScheduled
    , isMsg
    , isTick

    ) where

import           Oscoin.Prelude hiding (log, show)

import           Oscoin.Test.Crypto
import           Oscoin.Time.Chrono (toNewestFirst)

import           Oscoin.Consensus (ValidationError)
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain
                 (Blockchain, blocks, showBlockDigest, showChainDigest)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block
                 , BlockHash
                 , Sealed
                 , blockData
                 , blockHash
                 , blockHeader
                 , blockPrevHash
                 )
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (HasHashing, Hash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Protocol as Protocol
import qualified Oscoin.Storage as Storage
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Time

import           Test.Oscoin.DummyLedger
import           Test.QuickCheck hiding (sample)

import           Codec.Serialise (Serialise)
import qualified Data.Hashable as Hashable
import           Data.List (unlines)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Formatting (left, right, sformat, shown, stext, (%), (%.))
import qualified Formatting as F
import           System.Random
import           System.Random.Shuffle (shuffle')
import           Text.Show (Show(..))


-- DummyNodeId  ---------------------------------------------------------------

newtype DummyNodeId = DummyNodeId Word8
    deriving (Eq, Ord, Num, Hashable.Hashable)

deriving instance (HasHashing c, Hashable c Word8) => Hashable c DummyNodeId

instance Show DummyNodeId where
    show (DummyNodeId x) = show x

instance Arbitrary DummyNodeId where
    arbitrary = DummyNodeId <$> arbitrary

-- TestableNode ----------------------------------------------------------------

class (MonadMempool c DummyTx m) => TestableNode c s m a | a -> m, m -> a, a -> s where
    testableInit       :: DummyNodeId -> a
    testableTick       :: Timestamp -> m (Maybe (Block c DummyTx (Sealed c s)))
    testableRun        :: a -> m b -> (b, a)
    testableBlockStore :: Abstract.BlockStore c DummyTx s m
    testableProtocol   :: Protocol.Handle c DummyTx s m
    testableBestChain  :: a -> Blockchain c DummyTx s

testableLongestChain :: TestableNode c s m a => a -> [BlockHash c]
testableLongestChain =
      map blockHash
    . toNewestFirst
    . blocks
    . testableBestChain

testableIncludedTxs :: TestableNode c s m a => a -> [DummyTx]
testableIncludedTxs =
      concatMap (toList . blockData)
    . blocks
    . testableBestChain

testableShow :: (HasHashing c, TestableNode c s m a) => a -> Text
testableShow = showChainDigest . testableBestChain

-- Msg ------------------------------------------------------------------------

data Msg c s =
      BlockMsg    (Block c DummyTx (Sealed c s))
    -- ^ Message containing a 'Block'.
    | TxMsg       DummyTx
    -- ^ Message containing a @tx@.
    | ReqBlockMsg DummyNodeId (BlockHash c)
    -- ^ Message requesting a 'Block' with a certain hash. 'DummyNodeId' is the requesting node.
    | ResBlockMsg DummyNodeId (Block c DummyTx (Sealed c s))
    -- ^ Message sent in response to 'ReqBlockMsg'. 'DummyNodeId' is the requesting node.

deriving instance (Eq s, Eq (Hash c)) => Eq (Msg c s)

instance HasHashing c => Show (Msg c s) where
    show (BlockMsg  blk)   = F.formatToString (F.stext % " " % F.stext) "BlockMsg" (showBlockDigest blk)
    show (TxMsg     txs)   = "TxMsg " ++ show txs
    show (ReqBlockMsg n h) = "ReqBlockMsg " ++ "(" ++ show n ++ ") " ++ show (Crypto.compactHash h)
    show (ResBlockMsg n b) = "ResBlockMsg " ++ "(" ++ show n ++ ") " ++ T.unpack (showBlockDigest b)

-- TestNetwork -----------------------------------------------------------------

-- | The @TestNetwork s a@ models a set of nodes parameterized over the
-- node state @a@ and messages to be delivered between the nodes.
data TestNetwork c s a = TestNetwork
    { tnNodes      :: Map DummyNodeId a
    , tnScheduled  :: Set (Scheduled c s)
    -- ^ Initial set of scheduled events.
    , tnMsgs       :: Set (Scheduled c s)
    -- ^ Events that are to be delivered to the network
    , tnPartitions :: Partitions
    -- ^ Determines which nodes can send messages between them
    , tnLog        :: [Scheduled c s]
    -- ^ List of events the network has generated
    , tnLatencies  :: [Duration]
    -- ^ Infinite list of message latencies. When a node sends a
    -- message we take the first element to determine when the message
    -- gets delivered.
    , tnRng        :: StdGen
    , tnEval       :: DummyEval
    , tnValidate   :: DummyValidate c s
    , tnMsgCount   :: Int
    -- ^ Number of messages delivered in the network
    , tnLastTick   :: Timestamp
    }

-- | Test evaluator.
type DummyEval = Evaluator DummyState DummyTx ()

-- | Test block validator.
type DummyValidate c s =
    (Block c DummyTx (Sealed c s) -> Either (ValidationError c) ())

-- | Network partitions is a map of node id to a set of node ids _not_
-- reachable from that node id.
type Partitions = Map DummyNodeId (Set DummyNodeId)

instance (IsCrypto c, Serialise s) => Show (TestNetwork c s a) where
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


initNetwork :: TestableNode c s m a => TestNetwork c s b -> TestNetwork c s a
initNetwork tn =
    tn { tnNodes = Map.mapWithKey (\nodeId _ -> testableInit nodeId) (tnNodes tn) }


-- | Run the network simulation.
runNetwork
    :: ( IsCrypto c
       , Ord s
       , Serialise s
       , TestableNode c s m a
       )
    => Consensus.Config
    -> TestNetwork c s a
    -> TestNetwork c s a
runNetwork config tn@TestNetwork{tnScheduled} =
    runNetwork' config tn { tnMsgs = tnScheduled }

-- | Takes the next scheduled event from 'tnMsgs' and applies it to the
-- network.
runNetwork'
    :: ( IsCrypto c
       , Ord s
       , Serialise s
       , TestableNode c s m a
       )
    => Consensus.Config
    -> TestNetwork c s a
    -> TestNetwork c s a
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
    :: ( IsCrypto c
       , Ord s
       , Serialise s
       , TestableNode c s m a
       )
    => Consensus.Config
    -> Timestamp
    -> DummyNodeId
    -> Maybe (Msg c s)
    -> TestNetwork c s a
    -> TestNetwork c s a
deliver config tick to msg tn@TestNetwork{tnNodes, tnMsgCount, tnValidate}
    | Just node <- Map.lookup to tnNodes =
        let (outgoing, a) = testableRun node $ maybe (applyTick tick) (applyMessage config to tnValidate) msg
            tnMsgCount'   = case msg of Just (BlockMsg _) -> tnMsgCount + 1;
                                                        _ -> tnMsgCount
            tn'           = tn { tnNodes    = Map.insert to a tnNodes
                               , tnMsgCount = tnMsgCount'
                               }
         in scheduleMessages tick to outgoing tn'

    | otherwise = tn

applyTick :: TestableNode c s m a => Timestamp -> m [Msg c s]
applyTick t =
    map BlockMsg . maybeToList <$> testableTick t

-- | Apply a message to the node state. Returns a list of messages to send back
-- to the network.
applyMessage
    :: ( IsCrypto c
       , TestableNode c s m a
       , Serialise s
       , Ord s
       )
    => Consensus.Config
    -> DummyNodeId
    -> DummyValidate c s
    -> Msg c s
    -> m [Msg c s]
applyMessage _ _ _ msg@(TxMsg tx) = do
    let (blockStoreReader, _) = testableBlockStore
    result <- Storage.applyTx blockStoreReader tx
    pure $ case result of
        Storage.Applied _ -> [msg]
        _                 -> []
applyMessage config to validateBasic msg@(BlockMsg blk) = do
    let (blockStoreReader, _) = testableBlockStore
    let proto = testableProtocol
    result <- Storage.applyBlock proto validateBasic config blk
    parentMissing <- map isNothing (Abstract.lookupBlock blockStoreReader parentHash)

    -- If the parent block is missing, request it from the network.
    pure $ case result of
        Storage.Applied _
            | parentMissing -> [msg, ReqBlockMsg to parentHash]
            | otherwise     -> [msg]
        _ -> []
  where
    parentHash = blockPrevHash $ blockHeader blk
applyMessage _ _ _ (ReqBlockMsg from bh) = do
    let (blockStoreReader, _) = testableBlockStore
    mblk <- Abstract.lookupBlock blockStoreReader bh
    pure . maybeToList . map (ResBlockMsg from) $ mblk
applyMessage config to validate (ResBlockMsg _ blk) =
    applyMessage config to validate (BlockMsg blk)

-- | Schedules a list of messages to be delivered by the simulation to a set
-- of random nodes at some point in the future.
scheduleMessages
    :: forall c s a.
       ( IsCrypto c
       , Ord s
       )
    => Timestamp
    -> DummyNodeId
    -> [Msg c s]
    -> TestNetwork c s a
    -> TestNetwork c s a
scheduleMessages t from msgs tn@TestNetwork{..} =
    tn { tnMsgs = msgs', tnLog = log, tnLatencies = tnLatencies', tnRng = tnRng' }
  where
    (lats, tnLatencies') = splitAt (length messages) tnLatencies
    (rng, tnRng')        = split tnRng

    msgs' = Set.union (Set.fromList $ scheduled messages) tnMsgs
    log   = scheduled messages ++ tnLog

    messages :: [(Msg c s, DummyNodeId)]
    messages = concat [[(msg, r) | r <- recipients msg] | msg <- msgs]

    recipients :: Msg c s -> [DummyNodeId]
    recipients (ResBlockMsg id _) = [id]
    recipients _                  = sampledPeers 3

    scheduled :: [(Msg c s, DummyNodeId)] -> [Scheduled c s]
    scheduled [] = []
    scheduled xs =
        [ScheduledMessage (t `timeAdd` lat `timeAdd` (1 * nanoseconds)) to from msg
            | (lat, (msg, to)) <- zip lats xs]

    sampledPeers n = sample n (peers tn from)

    sample _ [] = []
    sample n xs = take n $ shuffle' xs (length xs) rng

-- | Returns the list of nodes that are reachable from the given node
-- in the network taking into account the network partition.
peers :: TestNetwork c s a -> DummyNodeId -> [DummyNodeId]
peers TestNetwork{..} node =
    let notReachable = fromMaybe Set.empty (Map.lookup node tnPartitions)
    in toList $ Set.delete node $ Map.keysSet tnNodes `Set.difference` notReachable


-- | A trivial network is a network without any nodes or scheduled
-- messages.
networkNonTrivial :: TestNetwork c s v -> Bool
networkNonTrivial TestNetwork{tnNodes, tnScheduled}
    | Map.null tnNodes = False
    | Set.null msgs    = False
    | otherwise        = True
  where
    msgs = Set.filter isMsg tnScheduled

-- Scheduled -------------------------------------------------------------------

-- | An event that is to occur in a Node network at a specific tick.
data Scheduled c s
    = ScheduledMessage Timestamp DummyNodeId DummyNodeId (Msg c s)
    -- ^ Message will be delivered from one node to another. First
    -- 'DummyNodeId' is the recipient, second the sender. Results in
    -- calling 'stepM' on the recipient.
    | ScheduledTick    Timestamp DummyNodeId
    -- ^ The given node will execute its 'tickM' function
    | Partition        Timestamp Partitions
    -- ^ Set the partition of the network
    | Heal             Timestamp
    -- ^ Remove any partitioning of the network
    deriving (Show)

deriving instance (IsCrypto c, Eq s) => Eq (Scheduled c s)

prettyScheduled :: IsCrypto c => Scheduled c s -> Text
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

instance (IsCrypto c, Ord s) => Ord (Scheduled c s) where
    s <= s' = scheduledTick s <= scheduledTick s'

scheduledTick :: Scheduled c s -> Timestamp
scheduledTick (ScheduledMessage t _ _ _) = t
scheduledTick (ScheduledTick t _)        = t
scheduledTick (Partition t _)            = t
scheduledTick (Heal t)                   = t

scheduledReceiver :: Scheduled c s -> Maybe DummyNodeId
scheduledReceiver (ScheduledMessage _ a _ _) = Just a
scheduledReceiver (ScheduledTick _ a)        = Just a
scheduledReceiver (Partition _ _)            = Nothing
scheduledReceiver (Heal _)                   = Nothing

isTick :: Scheduled c s -> Bool
isTick (ScheduledTick _ _) = True
isTick _                   = False

isMsg :: Scheduled c s -> Bool
isMsg ScheduledMessage{} = True
isMsg _                  = False
