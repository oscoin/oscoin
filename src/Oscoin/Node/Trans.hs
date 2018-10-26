-- | 'NodeT' is a monad transformer that combines all the storage
-- classes with consensus.
module Oscoin.Node.Trans
    ( NodeT
    , runNodeT
    , Config(..)
    , Handle(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Consensus)
import           Oscoin.Consensus.Class
                 (MonadClock(..), MonadQuery(..), MonadUpdate(..))
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.Data.Query
import           Oscoin.Environment
import qualified Oscoin.Logging as Log
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Storage.Block.Class
                 (MonadBlockStore(..), maximumChainBy)
import qualified Oscoin.Storage.Block.STM as BlockStore
import           Oscoin.Storage.Receipt (MonadReceiptStore)
import qualified Oscoin.Storage.Receipt as ReceiptStore

import qualified Radicle.Extended as Rad

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Lens.Micro


newtype NodeT tx s i m a = NodeT (ReaderT (Handle tx s i) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Handle tx s i)
             , MonadIO
             , MonadTrans
             , MFunctor
             )

runNodeT :: Handle tx s i -> NodeT tx s i m a -> m a
runNodeT env (NodeT ma) = runReaderT ma env

-- | Node static config.
data Config = Config
    { cfgEnv    :: Environment
    , cfgLogger :: Log.Logger
    }

-- | Node handle.
data Handle tx s i = Handle
    { hConfig       :: Config
    , hNodeId       :: i
    , hStateTree    :: STree.Handle s
    , hBlockStore   :: BlockStore.Handle tx s
    , hMempool      :: Mempool.Handle tx
    , hEval         :: Evaluator s tx Rad.Value
    , hConsensus    :: Consensus tx (NodeT tx s i IO)
    , hReceiptStore :: ReceiptStore.Handle tx Rad.Value
    }

-------------------------------------------------------------------------------

instance ReceiptStore.HasHandle tx Rad.Value (Handle tx s i) where
    handleL = lens hReceiptStore (\s hReceiptStore -> s { hReceiptStore })

instance (Hashable tx, Monad m, MonadIO m) => MonadMempool tx (NodeT tx s i m) where
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

instance (Monad m, MonadIO m, Ord tx, Hashable tx) => MonadBlockStore tx s (NodeT tx s i m) where
    storeBlock blk = do
        bs <- asks hBlockStore
        liftIO . atomically $ BlockStore.put bs blk

    lookupBlock     = withBlockStore . BlockStore.lookupBlock
    getGenesisBlock = withBlockStore   BlockStore.getGenesisBlock
    lookupTx        = withBlockStore . BlockStore.lookupTx
    orphans         = withBlockStore   BlockStore.orphans
    maximumChainBy  = withBlockStore . BlockStore.maximumChainBy

    {-# INLINE storeBlock     #-}
    {-# INLINE lookupBlock    #-}
    {-# INLINE orphans        #-}
    {-# INLINE maximumChainBy #-}

withBlockStore
    :: MonadIO m
    => (BlockStore.BlockStore tx s -> b)
    -> NodeT tx s i m b
withBlockStore f = do
    bs <- asks hBlockStore
    liftIO . atomically $
        BlockStore.for bs f

instance (Monad m, MonadIO m, Query s) => MonadQuery (NodeT tx s i m) where
    type Key (NodeT tx s i m) = STree.Path
    type Val (NodeT tx s i m) = QueryVal s

    queryM k = do
        st <- asks hStateTree
        lift $ STree.getPath st k
    {-# INLINE queryM #-}

instance (Monad m, MonadIO m) => MonadUpdate s (NodeT tx s i m) where
    updateM s = do
        st <- asks hStateTree
        liftIO . atomically $ STree.updateTree st s

instance MonadClock m => MonadClock (NodeT tx s i m)

instance (MonadIO m) => MonadReceiptStore tx Rad.Value (NodeT tx s i m) where
    addReceipt = ReceiptStore.addWithHandle
    lookupReceipt = ReceiptStore.lookupWithHandle
