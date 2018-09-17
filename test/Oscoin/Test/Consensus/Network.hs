module Oscoin.Test.Consensus.Network where

import           Oscoin.Prelude hiding (log)

import           Oscoin.Test.Consensus.Node

import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Nakamoto (NakamotoT, NakamotoEnv(..), runNakamotoT)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Simple (SimpleT, runSimpleT)
import qualified Oscoin.Consensus.Simple as Simple
import           Oscoin.Consensus.Evaluator (identityEval)
import           Oscoin.Crypto.Blockchain (Blockchain, fromBlockchain, showChainDigest)
import           Oscoin.Crypto.Blockchain.Block (BlockHeader(..), blockData, blockHeader)
import           Oscoin.Crypto.Hash (Hashed, hash)
import qualified Oscoin.Logging as Log
import           Oscoin.P2P (Msg(..))

import qualified Data.Hashable as Hashable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           System.Random
import           System.Random.Shuffle (shuffle')

-- TestableNode ----------------------------------------------------------------

class MonadProtocol DummyTx (TestableRun a) => TestableNode a where
    type TestableRun a   :: * -> *

    testableInit         :: TestNetwork b -> TestNetwork a

    testableRun          :: a -> (TestableRun a) b -> (b, a)
    testableLongestChain :: a -> [Hashed (BlockHeader ())]
    testableIncludedTxs  :: a -> [DummyTx]
    testableNodeAddr     :: a -> DummyNodeId
    testableShow         :: a -> String

-- Nakamoto Node ---------------------------------------------------------------

type NakamotoNode = NakamotoT DummyTx () (TestNodeT Identity)

data NakamotoNodeState = NakamotoNodeState
    { nakStdGen :: StdGen
    , nakNode   :: TestNodeState
    } deriving Show

instance TestableNode NakamotoNodeState where
    type TestableRun NakamotoNodeState = NakamotoNode

    testableInit = initNakamotoNodes
    testableRun  = runNakamotoNode

    testableLongestChain =
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
    ((!a, !g, ()), !tns) =
        runIdentity
            . runTestNodeT nakNode
            $ runNakamotoT env nakStdGen ma
    env = NakamotoEnv { nakEval = identityEval
                      , nakDifficulty = Nakamoto.minDifficulty
                      , nakMiner = mineBlock
                      , nakLogger = Log.noLogger }

-- | Mine a block header in 10% of the cases. The forged header does
-- not have a valid proof of work.
mineBlock :: StdGen -> BlockHeader a -> Maybe (BlockHeader a)
mineBlock stdGen bh@BlockHeader{..} =
    let r = fst $ randomR (0, 1) stdGen
        p = 0.1 :: Float
     in if r < p
           then Just bh
           else Nothing

nakamotoLongestChain :: NakamotoNodeState -> Blockchain DummyTx ()
nakamotoLongestChain =
      BlockStore.maximumChainBy Nakamoto.score . tnsBlockstore . nakNode

-- Simple Node -----------------------------------------------------------------

type SimpleNode = SimpleT DummyTx DummyNodeId (TestNodeT Identity)

data SimpleNodeState = SimpleNodeState
    { snsEnv  :: Simple.Env DummyNodeId
    , snsNode :: TestNodeState
    , snsLast :: Simple.LastTime
    } deriving Show

instance TestableNode SimpleNodeState where
    type TestableRun SimpleNodeState = SimpleNode

    testableInit = initSimpleNodes
    testableRun  = runSimpleNode

    testableLongestChain =
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

simpleBestChain :: SimpleNodeState -> Blockchain DummyTx ()
simpleBestChain =
      BlockStore.maximumChainBy (comparing Simple.chainScore)
    . tnsBlockstore
    . snsNode

-- TestNetwork -----------------------------------------------------------------

data TestNetwork a = TestNetwork
    { tnNodes      :: Map DummyNodeId a
    , tnMsgs       :: Set Scheduled
    , tnPartitions :: Partitions
    , tnLog        :: [Scheduled]
    , tnLatencies  :: [Tick]
    , tnRng        :: StdGen
    , tnMsgCount   :: Int
    , tnLastTick   :: Tick
    }

-- | Network partitions is a map of node id to a set of node ids _not_
-- reachable from that node id.
type Partitions = Map DummyNodeId (Set DummyNodeId)

instance Show (TestNetwork a) where
    show TestNetwork{..} =
        unlines [ "TestNetwork"
                , " nodes: "       ++ show (Map.keys tnNodes)
                , " last-tick: "   ++ show tnLastTick
                , " scheduled:\n"  ++ scheduled
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
scheduleMessages t from msgs tn@TestNetwork{tnNodes, tnMsgs, tnPartitions, tnLog, tnLatencies, tnRng} =
    tn { tnMsgs = msgs', tnLog = log, tnLatencies = tnLatencies', tnRng = tnRng' }
  where
    (lats, tnLatencies') = splitAt (length recipients) tnLatencies
    (rng, tnRng')        = split tnRng

    msgs' = Set.union (Set.fromList scheduled) tnMsgs
    log   = scheduled ++ tnLog

    peers      = Set.filter reachable . Set.delete from $ Map.keysSet tnNodes
    recipients = sample (toList peers)

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
    sample []  = []
    sample xs  = take 3 $ shuffle' xs (length xs) rng

    unreachables = Map.lookup from tnPartitions

    reachable _    | Map.null tnPartitions = True -- no partitions, all reachable
    reachable rcpt = case unreachables of
        Nothing -> True                           -- no one is partitioned from 'rcpt'
        Just ur -> Set.notMember rcpt ur          -- 'rcpt' is reachable iff not in the partition set

    scheduled =
            (\(lat, (to, msg)) -> ScheduledMessage (t + lat) to (from, msg))
        <$> zip lats [(rcpt, msg) | rcpt <- recipients, msg <- msgs]


networkNonTrivial :: TestNetwork v -> Bool
networkNonTrivial TestNetwork{tnNodes, tnMsgs}
    | Map.null tnNodes = False
    | Set.null msgs    = False
    | otherwise        = True
  where
    msgs = Set.filter isMsg tnMsgs

-- Scheduled -------------------------------------------------------------------

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
