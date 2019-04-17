-- | 'NodeT' is a monad transformer that combines all the storage
-- classes with consensus.
module Oscoin.Node.Trans
    ( NodeT
    , runNodeT
    , Config(..)
    , GlobalConfig(..)
    , Handle(..)
    , getBlockStoreReader
    , getLedger
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (MonadClock(..))
import           Oscoin.Configuration (Environment, Network)
import           Oscoin.Consensus (Consensus)
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Hash (Hash, Hashable)
import qualified Oscoin.Data.RadicleTx as RadicleTx
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.P2P.Types as P2P (Network)
import qualified Oscoin.Protocol as Protocol
import qualified Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Ledger as Ledger
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
    { hConfig    :: Config
    , hNodeId    :: i
    , hProtocol  :: Protocol.Handle c tx s IO
    , hMempool   :: Mempool.Handle c tx
    , hConsensus :: Consensus c tx s (NodeT c tx st s i IO)
    , hLedger    :: Ledger.Ledger c s tx RadicleTx.Message st IO
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

instance MonadClock m => MonadClock (NodeT c tx st s i m)

getBlockStoreReader
    :: (MonadIO m, MonadIO n)
    => NodeT c tx st s i n (Abstract.BlockStoreReader c tx s m)
getBlockStoreReader = Ledger.blockStoreReader <$> getLedger

getLedger
    :: (MonadIO m, MonadIO n)
    => NodeT c tx st s i m (Ledger.Ledger c s tx RadicleTx.Output st n)
getLedger = Ledger.hoist liftIO <$> asks hLedger
