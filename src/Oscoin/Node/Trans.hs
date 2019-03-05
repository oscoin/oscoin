-- | 'NodeT' is a monad transformer that combines all the storage
-- classes with consensus.
module Oscoin.Node.Trans
    ( NodeT
    , runNodeT
    , Config(..)
    , Handle(..)
    , withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Consensus)
import           Oscoin.Consensus.Class (MonadClock(..), MonadQuery(..))
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain.Block (StateHash)
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hash, Hashable)
import           Oscoin.Data.Query
import           Oscoin.Environment
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Receipt (MonadReceiptStore)
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore
import           Oscoin.Storage.State.Class (MonadStateStore(..))
import qualified Oscoin.Telemetry as Telemetry

import qualified Radicle.Extended as Rad

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
    , hBlockStore   :: Abstract.BlockStore c tx s IO
    , hMempool      :: Mempool.Handle c tx
    , hEval         :: Evaluator st tx Rad.Value
    , hConsensus    :: Consensus c tx s (NodeT c tx st s i IO)
    , hReceiptStore :: ReceiptStore.Handle c tx Rad.Value
    }

-------------------------------------------------------------------------------

instance Telemetry.HasTelemetry (Handle c tx st s i) where
    telemetryStoreL = to (cfgTelemetry . hConfig)

instance ReceiptStore.HasHandle c tx Rad.Value (Handle c tx st s i) where
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

withBlockStore
    :: MonadIO m
    => (Abstract.BlockStore c tx s IO -> IO b)
    -> NodeT c tx st s i m b
withBlockStore f = do
    bs <- asks hBlockStore
    liftIO $ f bs

instance ( Ord (StateHash c)
         , Monad m
         , MonadIO m
         , Query st
         , Hashable c st
         ) => MonadQuery (NodeT c tx st s i m) where
    type Key (NodeT c tx st s i m) = (StateHash c, STree.Path)
    type Val (NodeT c tx st s i m) = QueryVal st

    queryM (sh, k) = do
        result <- lookupState sh
        pure $ query k =<< result
    {-# INLINE queryM #-}

instance MonadClock m => MonadClock (NodeT c tx st s i m)

instance (Ord (Hash c), MonadIO m) => MonadReceiptStore c tx Rad.Value (NodeT c tx st s i m) where
    addReceipt = ReceiptStore.addWithHandle
    lookupReceipt = ReceiptStore.lookupWithHandle

instance (Ord (StateHash c), MonadIO m, Hashable c st) => MonadStateStore c st (NodeT c tx st s i m) where
    lookupState k = do
        h <- asks hStateStore
        lift $ StateStore.withHandle h (pure . StateStore.lookupState k)
    storeState st = do
        h <- asks hStateStore
        liftIO $ StateStore.storeStateIO h st

    {-# INLINE lookupState #-}
    {-# INLINE storeState #-}
