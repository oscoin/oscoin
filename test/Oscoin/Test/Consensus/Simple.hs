module Oscoin.Test.Consensus.Simple
    ( SimpleNodeState
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Mining (mineBlock)
import           Oscoin.Consensus.Simple
import           Oscoin.Crypto.Blockchain.Eval (identityEval)
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import           Oscoin.Storage.Block.Class (MonadBlockStore(..))
import           Oscoin.Storage.Receipt
import           Oscoin.Storage.State.Class (MonadStateStore(..))
import           Oscoin.Time

import           Oscoin.Test.Consensus.Class
import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Node

import           Control.Monad.State (modify')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Lens.Micro

type Position = (Int, Int)

type S = Map [Text] LByteString

data LastTime = LastTime
    { ltLastBlk :: Timestamp
    , ltLastAsk :: Timestamp
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

instance MonadMempool      tx    m => MonadMempool      tx    (SimpleT tx i m)
instance MonadBlockStore   tx () m => MonadBlockStore   tx () (SimpleT tx i m)
instance MonadReceiptStore tx () m => MonadReceiptStore tx () (SimpleT tx i m)
instance MonadStateStore   S     m => MonadStateStore    S    (SimpleT tx i m)

runSimpleT :: Position -> LastTime -> SimpleT tx i m a -> m (a, LastTime)
runSimpleT env lt (SimpleT ma) = runStateT (runReaderT ma env) lt

-- Simple Node -----------------------------------------------------------------

type SimpleNode = SimpleT DummyTx DummyNodeId (TestNodeT PoA Identity)

data SimpleNodeState = SimpleNodeState
    { snsPosition :: (Int, Int)
    , snsNode     :: TestNodeState PoA
    , snsLast     :: LastTime
    } deriving Show

instance HasTestNodeState PoA SimpleNodeState where
    testNodeStateL = lens snsNode (\s snsNode -> s { snsNode })


instance TestableNode PoA SimpleNode SimpleNodeState where
    testableTick tick = do
        position <- ask
        blk <- mineBlock (simpleConsensus position) identityEval tick
        reqs <- reconcileSimple tick
        pure $ maybeToList (BlockMsg <$> blk) <> (ReqBlockMsg <$> reqs)

    testableInit = initSimpleNodes
    testableRun  = runSimpleNode
    testableScore = const chainScore

simpleNode :: DummyNodeId -> Set DummyNodeId -> SimpleNodeState
simpleNode nid peers = SimpleNodeState
    { snsPosition = (ourOffset, nTotalPeers)
    , snsNode = emptyTestNodeState dummySeal nid
    , snsLast = LastTime epoch epoch
    }
  where
    dummySeal blk = blk $> ()
    nTotalPeers = 1 + Set.size peers
    ourOffset   = Set.size $ Set.filter (< nid) peers

initSimpleNodes :: TestNetwork PoA a -> TestNetwork PoA SimpleNodeState
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
