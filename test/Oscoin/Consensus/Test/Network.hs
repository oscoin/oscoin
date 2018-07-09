{-# LANGUAGE LambdaCase #-}

module Oscoin.Consensus.Test.Network where

import           Oscoin.Prelude hiding (log)

import           Oscoin.Consensus.Test.Node

import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Nakamoto (NakamotoT, runNakamotoT)
import           Oscoin.Consensus.Simple (SimpleT, runSimpleT)
import qualified Oscoin.Consensus.Simple as Simple
import           Oscoin.Crypto.Blockchain (Blockchain, fromBlockchain, height, showChainDigest)
import           Oscoin.Crypto.Blockchain.Block (BlockHeader, blockData, blockHeader)
import           Oscoin.Crypto.Hash (Hashed, hash)
import           Oscoin.P2P (Msg(..))

import qualified Data.Hashable as Hashable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           System.Random (StdGen, mkStdGen)

-- TestableNode ---------------------------------------------------------------

class ( Eq   (TestableResult a)
      , Show (TestableResult a)
      , MonadProtocol DummyTx (TestableRun a)
      ) => TestableNode a
  where
    type TestableResult a :: *
    type TestableRun    a :: * -> *

    testableInit        :: TestNetwork b -> TestNetwork a
    testableRun         :: a -> (TestableRun a) b -> (b, a)

    testablePostState   :: a -> [TestableResult a]

    testableIncludedTxs :: a -> [DummyTx]

    testableNodeAddr    :: a -> DummyNodeId
    testableShow        :: a -> String

-- Nakamoto Node ---------------------------------------------------------------

type NakamotoNode = NakamotoT DummyTx (TestNodeT Identity)

data NakamotoNodeState = NakamotoNodeState
    { nakStdGen :: StdGen
    , nakNode   :: TestNodeState
    } deriving Show

instance TestableNode NakamotoNodeState where
    type TestableResult NakamotoNodeState = Hashed BlockHeader
    type TestableRun    NakamotoNodeState = NakamotoNode

    testableInit = initNakamotoNodes
    testableRun  = runNakamotoNode

    testablePostState =
          map (hash . blockHeader)
        . toList . fromBlockchain
        . nakamotoLongestChain

    testableIncludedTxs =
          concatMap (toList . blockData)
        . toList . fromBlockchain
        . nakamotoLongestChain

    testableNodeAddr = tnsNodeId . nakNode

    testableShow = showChainDigest . nakamotoLongestChain

nakamotoNode :: DummyNodeId -> NakamotoNodeState
nakamotoNode nid = NakamotoNodeState
    { nakStdGen = mkStdGen (Hashable.hash nid)
    , nakNode   = emptyTestNodeState nid
    }

initNakamotoNodes :: TestNetwork a -> TestNetwork NakamotoNodeState
initNakamotoNodes tn@TestNetwork{tnNodes} =
    tn { tnNodes = Map.mapWithKey (const . nakamotoNode) tnNodes }

runNakamotoNode :: NakamotoNodeState -> NakamotoNode a -> (a, NakamotoNodeState)
runNakamotoNode s@NakamotoNodeState{..} ma =
    (a, s { nakStdGen = g, nakNode = tns })
  where
    ((!a, !g), !tns) =
        runIdentity
            . runTestNodeT nakNode
            $ runNakamotoT nakStdGen ma

nakamotoLongestChain :: NakamotoNodeState -> Blockchain DummyTx
nakamotoLongestChain =
      BlockStore.maximumChainBy (comparing height) . tnsBlockstore . nakNode

-- Simple Node -----------------------------------------------------------------

type SimpleNode = SimpleT DummyTx DummyNodeId (TestNodeT Identity)

data SimpleNodeState = SimpleNodeState
    { snsEnv  :: Simple.Env DummyNodeId
    , snsNode :: TestNodeState
    , snsLast :: Simple.LastTime
    } deriving Show

instance TestableNode SimpleNodeState where
    type TestableResult SimpleNodeState = Hashed BlockHeader
    type TestableRun    SimpleNodeState = SimpleNode

    testableInit = initSimpleNodes
    testableRun  = runSimpleNode

    testablePostState =
          map (hash . blockHeader)
        . toList . fromBlockchain
        . simpleBestChain

    testableIncludedTxs =
          concatMap (toList . blockData)
        . toList . fromBlockchain
        . simpleBestChain

    testableNodeAddr = tnsNodeId . snsNode

    testableShow = showChainDigest . simpleBestChain

simpleNode :: DummyNodeId -> Set DummyNodeId -> SimpleNodeState
simpleNode nid peers = SimpleNodeState
    { snsEnv  = Simple.mkEnv nid peers
    , snsNode = emptyTestNodeState nid
    , snsLast = Simple.LastTime 0 0
    }

initSimpleNodes :: TestNetwork a -> TestNetwork SimpleNodeState
initSimpleNodes tn@TestNetwork{tnNodes} =
    let nodes   = Map.keysSet tnNodes
        !nodes' = Map.fromList
                . map (\node -> (node, simpleNode node (Set.delete node nodes)))
                $ toList nodes
     in tn { tnNodes = nodes' }

runSimpleNode :: SimpleNodeState -> SimpleNode a -> (a, SimpleNodeState)
runSimpleNode s@SimpleNodeState{..} ma = (a, s { snsNode = tns, snsLast = lt })
  where
    ((!a, !lt), !tns) =
        runIdentity
            . runTestNodeT snsNode
            $ runSimpleT snsEnv snsLast ma

simpleBestChain :: SimpleNodeState -> Blockchain DummyTx
simpleBestChain =
      BlockStore.maximumChainBy (comparing Simple.chainScore)
    . tnsBlockstore
    . snsNode

-- TestNetwork ----------------------------------------------------------------

data TestNetwork a = TestNetwork
    { tnNodes      :: Map DummyNodeId a
    , tnMsgs       :: Set Scheduled
    , tnPartitions :: Partitions
    , tnLog        :: [Scheduled]
    , tnLatencies  :: [Tick]
    , tnMsgCount   :: Int
    , tnLastTick   :: Tick
    }

-- | Network partitions is a map of node id to a set of node ids _not_
-- reachable from that node id.
type Partitions = Map DummyNodeId (Set DummyNodeId)

instance Show (TestNetwork a) where
    show TestNetwork{..} =
        unlines [ "TestNetwork"
                , " nodes: "      ++ show (Map.keys tnNodes)
                , " scheduled:\n" ++ scheduled
                ]
      where
        scheduled = unlines
            ["  " ++ show msg | msg <- filter (not . isTick) (toList tnMsgs)]

runNetwork :: TestableNode a => TestNetwork a -> TestNetwork a
runNetwork tn@TestNetwork{tnMsgs, tnLastTick}
    | Just (sm, sms)   <- Set.minView tnMsgs
    , scheduledTick sm <= tnLastTick
    , tn'              <- tn { tnMsgs = sms } =
        runNetwork $ case sm of
            ScheduledTick    tick to         -> deliver tick to Nothing    tn'
            ScheduledMessage tick to (_,msg) -> deliver tick to (Just msg) tn'
            Partition _ ps                   -> tn' { tnPartitions = ps     }
            Heal _                           -> tn' { tnPartitions = mempty }

    | otherwise = tn

deliver
    :: TestableNode a
    => Tick
    -> DummyNodeId
    -> Maybe (Msg DummyTx)
    -> TestNetwork a
    -> TestNetwork a
deliver tick to msg tn@TestNetwork{tnNodes, tnMsgCount}
    | Just node <- Map.lookup to tnNodes =
        let (outgoing, a) = testableRun node $ maybe (tickM tick) (stepM tick) msg
            tnMsgCount'   = case msg of Just (BlockMsg _) -> tnMsgCount + 1;
                                                        _ -> tnMsgCount
            tn'           = tn { tnNodes    = Map.insert to a tnNodes
                               , tnMsgCount = tnMsgCount'
                               }
         in scheduleMessages tick to outgoing tn'

    | otherwise = tn

scheduleMessages
    :: Tick
    -> DummyNodeId
    -> [Msg DummyTx]
    -> TestNetwork a
    -> TestNetwork a
scheduleMessages t from msgs tn@TestNetwork{tnNodes, tnMsgs, tnPartitions, tnLog, tnLatencies} =
    tn { tnMsgs = msgs', tnLog = log, tnLatencies = tnLatencies' }
  where
    (lats, tnLatencies') = splitAt (Set.size broadcast) tnLatencies

    msgs' = Set.union (Set.fromList scheduled) tnMsgs
    log   = scheduled ++ tnLog

    broadcast = Set.filter reachable . Set.delete from $ Map.keysSet tnNodes

    unreachables = Map.lookup from tnPartitions

    reachable _    | Map.null tnPartitions = True -- no partitions, all reachable
    reachable rcpt = case unreachables of
        Nothing -> True                           -- no one is partitioned from 'rcpt'
        Just ur -> Set.notMember rcpt ur          -- 'rcpt' is reachable iff not in the partition set

    scheduled =
            (\(lat, (to, msg)) -> ScheduledMessage (t + lat) to (from, msg))
        <$> zip lats [(rcpt, msg) | rcpt <- toList broadcast, msg <- msgs]


networkNonTrivial :: TestNetwork v -> Bool
networkNonTrivial TestNetwork{tnNodes, tnMsgs}
    | Map.null tnNodes = False
    | Set.null msgs    = False
    | otherwise        = True
  where
    msgs = Set.filter isMsg tnMsgs

-- Scheduled ------------------------------------------------------------------

data Scheduled =
      ScheduledMessage Tick DummyNodeId (DummyNodeId, Msg DummyTx)
    | ScheduledTick    Tick DummyNodeId
    | Partition        Tick Partitions
    | Heal             Tick
    deriving (Eq, Show)

instance Ord Scheduled where
    s <= s' = scheduledTick s <= scheduledTick s'

scheduledTick :: Scheduled -> Tick
scheduledTick (ScheduledMessage t _ _) = t
scheduledTick (ScheduledTick t _)      = t
scheduledTick (Partition t _)          = t
scheduledTick (Heal t)                 = t

scheduledReceivers :: Scheduled -> [DummyNodeId]
scheduledReceivers (ScheduledMessage _ a _) = [a]
scheduledReceivers (ScheduledTick _ a)      = [a]
scheduledReceivers (Partition _ _)          = [] -- XXX
scheduledReceivers (Heal _)                 = []

scheduledSender :: Scheduled -> Maybe DummyNodeId
scheduledSender (ScheduledMessage _ _ (a, _)) = Just a
scheduledSender _                             = Nothing

scheduledMessage :: Scheduled -> Maybe (DummyNodeId, Msg DummyTx)
scheduledMessage (ScheduledMessage _ _ x) = Just x
scheduledMessage _                        = Nothing

isTick :: Scheduled -> Bool
isTick (ScheduledTick _ _) = True
isTick _                   = False

isMsg :: Scheduled -> Bool
isMsg ScheduledMessage{} = True
isMsg _                  = False
