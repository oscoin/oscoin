-- | 'NodeT' is a monad transformer that combines all the storage
-- classes with consensus.
module Oscoin.Node.Trans
    ( NodeT
    , runNodeT
    , Config(..)
    , GlobalConfig(..)
    , Handle(..)
    , getBlockStoreReader
    , getStateStore
    , getReceiptStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (MonadClock(..))
import           Oscoin.Configuration (Environment, Network)
import           Oscoin.Consensus (Consensus)
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hash, Hashable)
import qualified Oscoin.Data.RadicleTx as RadicleTx
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.P2P.Types as P2P (Network)
import qualified Oscoin.Protocol as Protocol
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.ContentStore
import           Oscoin.Storage.HashStore
import           Oscoin.Storage.Receipt
import qualified Oscoin.Telemetry as Telemetry

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Lens.Micro


newtype NodeT c tx st s i m a = NodeT (ReaderT (Handle c tx st s i) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Handle c tx st s i)
             , MonadIO
             , MonadTrans
             , MFunctor
             )

runNodeT :: Handle c tx st s i -> NodeT c tx st s i m a -> m a
runNodeT env (NodeT ma) = runReaderT ma env

-- | Node static config.
data Config = Config
    { cfgGlobalConfig    :: GlobalConfig
    , cfgTelemetry       :: Telemetry.Handle
    , cfgConsensusConfig :: Consensus.Config
    }

data GlobalConfig = GlobalConfig
    { globalEnv             :: Environment
    , globalLogicalNetwork  :: Network
    , globalPhysicalNetwork :: P2P.Network
    }

-- | Node handle.
data Handle c tx st s i = Handle
    { hConfig       :: Config
    , hNodeId       :: i
    , hStateStore   :: HashStore c st IO
    , hBlockStore   :: Abstract.BlockStoreReader c tx s IO
    , hProtocol     :: Protocol.Handle c tx s IO
    , hMempool      :: Mempool.Handle c tx
    , hEval         :: Evaluator st tx RadicleTx.Message
    , hConsensus    :: Consensus c tx s (NodeT c tx st s i IO)
    , hReceiptStore :: ReceiptStore c tx RadicleTx.Output IO
    }

-------------------------------------------------------------------------------

instance Telemetry.HasTelemetry (Handle c tx st s i) where
    telemetryStoreL = to (cfgTelemetry . hConfig)

instance ( Ord (Hash c)
         , Hashable c tx
         , Monad m
         , MonadIO m
         ) => MonadMempool c tx (NodeT c tx st s i m) where
    addTxs txs = asks hMempool >>= liftIO . atomically . (`Mempool.insertMany` txs)
    getTxs     = asks hMempool >>= liftIO . atomically . Mempool.toList
    delTxs txs = asks hMempool >>= liftIO . atomically . (`Mempool.removeMany` txs)
    numTxs     = asks hMempool >>= liftIO . atomically . Mempool.size
    lookupTx h = asks hMempool >>= liftIO . atomically . (`Mempool.lookup` h)
    subscribe  = asks hMempool >>= liftIO . atomically . Mempool.subscribe

    {-# INLINE addTxs    #-}
    {-# INLINE getTxs    #-}
    {-# INLINE delTxs    #-}
    {-# INLINE numTxs    #-}
    {-# INLINE subscribe #-}

getBlockStoreReader
    :: (MonadIO m, MonadIO n)
    => NodeT c tx st s i n (Abstract.BlockStoreReader c tx s m)
getBlockStoreReader = do
    bs <- asks hBlockStore
    pure $ Abstract.hoistBlockStoreReader liftIO bs

instance MonadClock m => MonadClock (NodeT c tx st s i m)

getReceiptStore :: (Monad m, MonadIO n) => NodeT c tx st s i m (ReceiptStore c tx RadicleTx.Output n)
getReceiptStore = hoistContentStore liftIO <$> asks hReceiptStore

getStateStore :: (Monad m, MonadIO n) => NodeT c tx st s i m (HashStore c st n)
getStateStore = hoistHashStore liftIO <$> asks hStateStore
