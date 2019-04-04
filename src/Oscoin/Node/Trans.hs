-- | 'NodeT' is a monad transformer that combines all the storage
-- classes with consensus.
module Oscoin.Node.Trans
    ( NodeT
    , runNodeT
    , Config(..)
    , Handle(..)
    , getBlockStoreReader
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (MonadClock(..))
import           Oscoin.Configuration (Environment)
import           Oscoin.Consensus (Consensus)
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hash, Hashable)
import qualified Oscoin.Data.RadicleTx as RadicleTx
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Protocol as Protocol
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Receipt (MonadReceiptStore)
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore
import           Oscoin.Storage.State.Class (MonadStateStore(..))
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
    { cfgEnv             :: Environment
    , cfgTelemetry       :: Telemetry.Handle
    , cfgNoEmptyBlocks   :: Bool
    , cfgConsensusConfig :: Consensus.Config
    }

-- | Node handle.
data Handle c tx st s i = Handle
    { hConfig       :: Config
    , hNodeId       :: i
    , hStateStore   :: StateStore.Handle c st
    , hBlockStore   :: Abstract.BlockStoreReader c tx s IO
    , hProtocol     :: Protocol.Handle c tx s IO
    , hMempool      :: Mempool.Handle c tx
    , hEval         :: Evaluator st tx RadicleTx.Message
    , hConsensus    :: Consensus c tx s (NodeT c tx st s i IO)
    , hReceiptStore :: ReceiptStore.Handle c tx RadicleTx.Output
    }

-------------------------------------------------------------------------------

instance Telemetry.HasTelemetry (Handle c tx st s i) where
    telemetryStoreL = to (cfgTelemetry . hConfig)

instance ReceiptStore.HasHandle c tx RadicleTx.Output (Handle c tx st s i) where
    handleL = lens hReceiptStore (\s hReceiptStore -> s { hReceiptStore })

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

instance (Ord (Hash c), MonadIO m) => MonadReceiptStore c tx RadicleTx.Output (NodeT c tx st s i m) where
    addReceipt = ReceiptStore.addWithHandle
    lookupReceipt = ReceiptStore.lookupWithHandle

instance (MonadIO m, Hashable c st) => MonadStateStore c st (NodeT c tx st s i m) where
    lookupState k = do
        h <- asks hStateStore
        lift $ StateStore.withHandle h (pure . StateStore.lookupState k)
    storeState st = do
        h <- asks hStateStore
        liftIO $ StateStore.storeStateIO h st

    {-# INLINE lookupState #-}
    {-# INLINE storeState #-}
