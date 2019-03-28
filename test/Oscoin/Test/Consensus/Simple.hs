{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Test.Consensus.Simple
    ( SimpleNodeState
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Mining (mineBlock)
import           Oscoin.Consensus.Simple
import           Oscoin.Crypto.Blockchain.Block (sealBlock)
import           Oscoin.Crypto.Blockchain.Eval (identityEval)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Receipt
import           Oscoin.Storage.State.Class (MonadStateStore(..))
import           Oscoin.Time

import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Node
import           Oscoin.Test.Crypto

import           Control.Monad.State (modify')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Lens.Micro

type Position = (Int, Int)

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

instance (IsCrypto c, MonadMempool c tx m) => MonadMempool c tx   (SimpleT tx i m)
instance MonadReceiptStore c tx () m => MonadReceiptStore c tx () (SimpleT tx i m)
instance MonadStateStore   c st    m => MonadStateStore   c st    (SimpleT tx i m)

runSimpleT :: Position -> LastTime -> SimpleT tx i m a -> m (a, LastTime)
runSimpleT env lt (SimpleT ma) = runStateT (runReaderT ma env) lt

-- Simple Node -----------------------------------------------------------------

type SimpleNode c = SimpleT DummyTx DummyNodeId (TestNodeT c PoA Identity)

data SimpleNodeState c = SimpleNodeState
    { snsPosition :: (Int, Int)
    , snsNode     :: TestNodeState c PoA
    , snsLast     :: LastTime
    }

deriving instance Show (Crypto.Hash c) => Show (SimpleNodeState c)

instance HasTestNodeState c PoA (SimpleNodeState c) where
    testNodeStateL = lens snsNode (\s snsNode -> s { snsNode })

instance LiftTestNodeT c PoA (SimpleNode c) where
    liftTestNodeT = lift

instance (IsCrypto c) => TestableNode c PoA (SimpleNode c) (SimpleNodeState c) where
    testableTick tick = do
        position <- ask
        withTestBlockStore $ \(publicAPI, privateAPI) -> do
            -- NOTE (adn): We are bypassing the protocol at the moment, but we
            -- probably shouldn't.
            res <- mineBlock publicAPI (simpleConsensus position) identityEval tick
            case res of
              Nothing -> pure Nothing
              Just blk -> do
                  Abstract.insertBlock privateAPI blk
                  pure (Just blk)

    testableInit = initSimpleNodes
    testableRun  = runSimpleNode
    testableScore = const chainScore

simpleNode
    :: (IsCrypto c)
    => DummyNodeId
    -> Set DummyNodeId
    -> SimpleNodeState c
simpleNode nid peers = SimpleNodeState
    { snsPosition = (ourOffset, nTotalPeers)
    , snsNode = emptyTestNodeState (sealBlock ()) nid
    , snsLast = LastTime epoch epoch
    }
  where
    nTotalPeers = 1 + Set.size peers
    ourOffset   = Set.size $ Set.filter (< nid) peers

initSimpleNodes
    :: (IsCrypto c)
    => TestNetwork c PoA a
    -> TestNetwork c PoA (SimpleNodeState c)
initSimpleNodes tn@TestNetwork{tnNodes} =
    let nodes   = Map.keysSet tnNodes
        !nodes' = Map.fromList
                . map (\node -> (node, simpleNode node (Set.delete node nodes)))
                $ toList nodes
     in tn { tnNodes = nodes' }

runSimpleNode :: SimpleNodeState c -> SimpleNode c a -> (a, SimpleNodeState c)
runSimpleNode s@SimpleNodeState{..} ma = (a, s { snsNode = tns, snsLast = lt })
  where
    ((a, lt), tns) =
        runIdentity
            . runTestNodeT snsNode
            $ runSimpleT snsPosition snsLast ma
