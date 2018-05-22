module Oscoin.Consensus.Test.Network where

import           Oscoin.Prelude hiding (log)
import           Oscoin.Consensus.Test.Node
import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Simple
import           Oscoin.Consensus.Simple.Arbitrary ()
import           Oscoin.Consensus.Nakamoto.Arbitrary ()
import           Oscoin.Consensus.Nakamoto (nakamoto, Nakamoto(..), NodeMsg(..), longestChain)
import           Oscoin.Crypto.Blockchain.Block (genesisBlock, blockHeader, BlockHeader)
import           Oscoin.Crypto.Blockchain (showChainDigest, fromBlockchain)
import           Oscoin.Crypto.Hash (Hashed, Hashable, hash)

import           Data.Binary (Binary)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           System.Random (mkStdGen, randomRs)

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
      , Eq (TestableResult a)
      , Show (TestableResult a)
      , Protocol a ) => TestableNode a where
    type TestableTx a :: *
    type TestableResult a :: *

    testableNode :: Addr a -> [Addr a] -> a
    testablePostState :: a -> [TestableResult a]
    testablePreState :: a -> Msg a -> [TestableTx a]
    testableNodeAddr :: a -> Addr a
    testableShow :: a -> String


instance (Show tx, Arbitrary tx, Ord tx) => TestableNode (TestNode tx) where
    type TestableTx (TestNode tx) = tx
    type TestableResult (TestNode tx) = tx

    testableNode addr = TestNode addr []
    testablePreState _ tx = [tx]
    testablePostState (TestNode _ s _) = s
    testableNodeAddr (TestNode a _ _) = a
    testableShow _ = "-"

instance (Show tx, Arbitrary tx, Ord tx) => TestableNode (BufferedTestNode tx) where
    type TestableTx (BufferedTestNode tx) = tx
    type TestableResult (BufferedTestNode tx) = tx

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
    testableShow _ = "-"

instance (Binary tx, Show tx, Arbitrary tx, Ord tx, Hashable tx) => TestableNode (SimpleNode tx) where
    type TestableTx (SimpleNode tx) = tx
    type TestableResult (SimpleNode tx) = Hashed BlockHeader

    testableNode addr peers = SimpleNode
        { snAddr    = addr
        , snPeers   = peers
        , snBuffer  = mempty
        , snLastBlk = 0
        , snLastAsk = 0
        , snStore   = genesisBlockStore (genesisBlock 0 [])
        }

    testablePreState _ (ClientTx tx) = [tx]
    testablePreState _ _             = []

    testablePostState = map (hash . blockHeader) . toList . fromBlockchain . bestChain . snStore

    testableNodeAddr = snAddr
    testableShow SimpleNode{..} = showChainDigest $ bestChain $ snStore

instance (Binary tx, Show tx, Arbitrary tx, Ord tx, Hashable tx) => TestableNode (Nakamoto tx) where
    type TestableTx (Nakamoto tx) = tx
    type TestableResult (Nakamoto tx) = Hashed BlockHeader

    testableNode addr peers =
        nakamoto addr (genesisBlock 0 []) peers (randomRs (0, 1) rng)
      where
        rng = mkStdGen (fromIntegral addr)

    testablePreState _ (TxMsg tx) = [tx]
    testablePreState _ _          = []

    testablePostState = map (hash . blockHeader) . toList . fromBlockchain . longestChain
    testableNodeAddr = nkAddr
    testableShow = showChainDigest . longestChain

-- TestNetwork ----------------------------------------------------------------

data TestNetwork a = TestNetwork
    { tnNodes      :: Map (Addr a) a
    , tnMsgs       :: Set (Scheduled a)
    , tnPartitions :: Partitions a
    , tnLog        :: [Scheduled a]
    , tnLatencies  :: [Tick]
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
runNetwork tn@TestNetwork{tnMsgs}
    | Just (sm, sms) <- Set.minView tnMsgs
    , tn' <- tn { tnMsgs = sms } =
        runNetwork $ case sm of
            ScheduledTick tick to        -> deliver tick to Nothing tn'
            ScheduledMessage tick to msg -> deliver tick to (Just msg) tn'
            Partition _ ps               -> tn' { tnPartitions = ps }
            Heal _                       -> tn' { tnPartitions = mempty }
    | otherwise = tn

deliver
    :: (TestableNode a)
    => Tick
    -> Addr a
    -> Maybe (Addr a, Msg a)
    -> TestNetwork a
    -> TestNetwork a
deliver tick to msg tn@TestNetwork{tnNodes}
    | Just node <- Map.lookup to tnNodes =
        let (node', msgs) = step node tick msg
            tn'           = tn { tnNodes = Map.insert to node' tnNodes }
         in scheduleMessages tick to msgs tn'
    | otherwise =
        tn

scheduleMessages
    :: TestableNode a
    => Tick
    -> Addr a
    -> [(Addr a, Msg a)]
    -> TestNetwork a
    -> TestNetwork a
scheduleMessages t from msgs tn@TestNetwork{tnMsgs, tnPartitions, tnLog, tnLatencies} =
    let
        msgs'                = Set.union (Set.fromList scheduled) tnMsgs
        (lats, tnLatencies') = splitAt (length msgs) tnLatencies
        log                  = scheduled ++ tnLog
        scheduled            = [ScheduledMessage (t + lat) to (from, msg) | (lat, (to, msg)) <- zip lats msgs, reachable to]
        reachable to         = maybe True not $
            Set.member to <$> Map.lookup from tnPartitions
     in tn { tnMsgs = msgs', tnLog = log, tnLatencies = tnLatencies' }

networkNonTrivial :: TestNetwork v -> Bool
networkNonTrivial (TestNetwork ns ms _ _ _)
    | Map.null ns = False
    | Set.null ms = False
    | otherwise   = True

-- Scheduled ------------------------------------------------------------------

data Scheduled a =
      ScheduledMessage Tick (Addr a) (Addr a, Msg a)
    | ScheduledTick    Tick (Addr a)
    | Partition        Tick (Partitions a)
    | Heal             Tick

deriving instance TestableNode a => Show (Scheduled a)
deriving instance TestableNode a => Eq (Scheduled a)

instance TestableNode a => Ord (Scheduled a) where
    s <= s' = scheduledTick s <= scheduledTick s'

scheduledTick :: Scheduled a -> Tick
scheduledTick (ScheduledMessage t _ _) = t
scheduledTick (ScheduledTick t _)      = t
scheduledTick (Partition t _)          = t
scheduledTick (Heal t)                 = t

scheduledReceivers :: Scheduled a -> [Addr a]
scheduledReceivers (ScheduledMessage _ a _) = [a]
scheduledReceivers (ScheduledTick _ a)      = [a]
scheduledReceivers (Partition _ _)          = [] -- XXX
scheduledReceivers (Heal _)                 = []

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
