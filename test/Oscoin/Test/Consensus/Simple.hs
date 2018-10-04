module Oscoin.Test.Consensus.Simple
    ( SimpleNodeState
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..))
import           Oscoin.Consensus.Evaluator
import           Oscoin.Consensus.Mining (mineBlock)
import           Oscoin.Consensus.Simple
import           Oscoin.Node.Mempool.Class (MonadMempool(..))

import           Oscoin.Test.Consensus.Class
import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Node

import           Control.Monad.State (modify')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Lens.Micro

type Position = (Int, Int)

data LastTime = LastTime
    { ltLastBlk :: Tick
    , ltLastAsk :: Tick
    } deriving Show

newtype SimpleT tx i m a = SimpleT (ReaderT Position (StateT LastTime m) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Position
             , MonadState  LastTime
             )

instance MonadTrans (SimpleT tx i) where
    lift = SimpleT . lift . lift
    {-# INLINE lift #-}

instance Monad m => MonadLastTime (SimpleT tx i m) where
    getLastBlockTick      = gets ltLastBlk
    setLastBlockTick tick = modify' (\s -> s { ltLastBlk = tick })

    getLastAskTick        = gets ltLastAsk
    setLastAskTick tick   = modify' (\s -> s { ltLastAsk = tick })

instance MonadMempool    tx    m => MonadMempool    tx    (SimpleT tx i m)
instance MonadBlockStore tx () m => MonadBlockStore tx () (SimpleT tx i m)

runSimpleT :: Position -> LastTime -> SimpleT tx i m a -> m (a, LastTime)
runSimpleT env lt (SimpleT ma) = runStateT (runReaderT ma env) lt

-- Simple Node -----------------------------------------------------------------

type SimpleNode = SimpleT DummyTx DummyNodeId (TestNodeT Identity)

data SimpleNodeState = SimpleNodeState
    { snsPosition :: (Int, Int)
    , snsNode     :: TestNodeState
    , snsLast     :: LastTime
    } deriving Show

instance HasTestNodeState SimpleNodeState where
    testNodeStateL = lens snsNode (\s snsNode -> s { snsNode })


instance TestableNode SimpleNode SimpleNodeState where
    testableTick tick = do
        let time = timeAdd epoch tick
        position <- ask
        blk <- mineBlock (simpleConsensus position) identityEval time
        reqs <- reconcileSimple time
        pure $ maybeToList (BlockMsg <$> blk) <> (ReqBlockMsg <$> reqs)

    testableInit = initSimpleNodes
    testableRun  = runSimpleNode
    testableScore = const chainScore

simpleNode :: DummyNodeId -> Set DummyNodeId -> SimpleNodeState
simpleNode nid peers = SimpleNodeState
    { snsPosition = (ourOffset, nTotalPeers)
    , snsNode = emptyTestNodeState nid
    , snsLast = LastTime epoch epoch
    }
  where
    nTotalPeers = 1 + Set.size peers
    ourOffset   = Set.size $ Set.filter (< nid) peers

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
    ((a, lt), tns) =
        runIdentity
            . runTestNodeT snsNode
            $ runSimpleT snsPosition snsLast ma
