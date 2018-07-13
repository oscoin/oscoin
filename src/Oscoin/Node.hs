module Oscoin.Node
    ( Config (..)
    , Handle
    , NodeT

    , withNode
    , open
    , close

    , runNodeT

    , run
    , step

    , getMempool
    , getAccountPath
    , getPath
    ) where

import           Oscoin.Prelude

import           Oscoin.Account (AccId, Account, pattern AccountsPrefix)
import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..))
import           Oscoin.Consensus.Class (MonadClock(..), MonadProtocol(..), MonadQuery(..))
import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.Environment
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Tree as STree
import           Oscoin.P2P (MonadNetwork(..), Msg)
import qualified Oscoin.Storage.Block as BlockStore

import           Control.Exception.Safe (bracket)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Network.Socket as NS

-- | Node static config.
data Config = Config
    { cfgServiceName :: NS.ServiceName
    , cfgPeers       :: [(NS.HostName, NS.ServiceName)]
    , cfgEnv         :: Environment
    , cfgAccounts    :: [(AccId, Account)]
    }

-- | Node handle.
data Handle tx i = Handle
    { hConfig     :: Config
    , hNodeId     :: i
    , hStateTree  :: STree.Handle
    , hBlockStore :: BlockStore.Handle tx
    , hMempool    :: Mempool.Handle tx
    }

withNode
    :: Config
    -> i
    -> Mempool.Handle tx
    -> STree.Handle
    -> BlockStore.Handle tx
    -> (Handle tx i -> IO c)
    -> IO c
withNode cfg i mem str blk = bracket (open cfg i mem str blk) close

-- | Connect to state storage.
open :: Config
     -> i
     -> Mempool.Handle tx
     -> STree.Handle
     -> BlockStore.Handle tx
     -> IO (Handle tx i)
open hConfig hNodeId hMempool hStateTree hBlockStore =
    pure Handle{..}

-- | Close the connection to state storage.
close :: Handle tx i -> IO ()
close = const $ pure ()

run :: ( MonadNetwork  tx m
       , MonadProtocol tx m
       , MonadClock       m
       )
    => proxy tx
    -> m ()
run = forever . step

step :: forall proxy tx m.
        ( MonadNetwork  tx m
        , MonadProtocol tx m
        , MonadClock       m
        )
     => proxy tx
     -> m ()
step _ = do
    r <- recvM :: m (Msg tx)
    t <- currentTick
    o <- stepM t r
    sendM o

-------------------------------------------------------------------------------

newtype NodeT tx i m a = NodeT (ReaderT (Handle tx i) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Handle tx i)
             , MonadTrans
             )

runNodeT :: Handle tx i -> NodeT tx i m a -> m a
runNodeT env (NodeT ma) = runReaderT ma env

instance (Hashable tx, Monad m, MonadSTM m) => MonadMempool tx (NodeT tx i m) where
    addTxs txs = asks hMempool >>= (`Mempool.insertMany` txs)
    getTxs     = asks hMempool >>= Mempool.toList
    delTxs txs = asks hMempool >>= (`Mempool.removeMany` txs)
    numTxs     = asks hMempool >>= Mempool.size
    lookupTx h = asks hMempool >>= (`Mempool.lookup` h)
    subscribe  = asks hMempool >>= Mempool.subscribe

    {-# INLINE addTxs    #-}
    {-# INLINE getTxs    #-}
    {-# INLINE delTxs    #-}
    {-# INLINE numTxs    #-}
    {-# INLINE subscribe #-}

instance (Monad m, MonadSTM m, Ord tx, Hashable tx) => MonadBlockStore tx (NodeT tx i m) where
    storeBlock blk = do
        bs <- asks hBlockStore
        BlockStore.put bs blk

    lookupBlock hdr = do
        bs <- asks hBlockStore
        BlockStore.for bs $
            BlockStore.lookupBlock hdr

    lookupTx tx = do
        bs <- asks hBlockStore
        BlockStore.for bs $
            BlockStore.lookupTx tx

    orphans = do
        bs <- asks hBlockStore
        BlockStore.for bs $
            BlockStore.orphans

    maximumChainBy cmp = do
        bs <- asks hBlockStore
        BlockStore.for bs $
            BlockStore.maximumChainBy cmp

    {-# INLINE storeBlock     #-}
    {-# INLINE lookupBlock    #-}
    {-# INLINE orphans        #-}
    {-# INLINE maximumChainBy #-}

instance (Monad m, MonadIO m) => MonadQuery (NodeT tx i m) where
    type Key (NodeT tx i m) = STree.Path
    type Val (NodeT tx i m) = STree.Val

    queryM k = do
        st <- asks hStateTree
        lift $ STree.get st k
    {-# INLINE queryM #-}

instance MonadClock m => MonadClock (NodeT tx i m) where
    currentTick = lift currentTick
    {-# INLINE currentTick #-}

instance MonadIO m => MonadIO (NodeT tx i m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

-------------------------------------------------------------------------------

getMempool :: (Monad m, MonadSTM m) => NodeT tx i m (Mempool tx)
getMempool = asks hMempool >>= Mempool.snapshot

getAccountPath :: MonadIO m => AccId -> STree.Path -> NodeT tx i m (Maybe STree.Val)
getAccountPath acc path =
    getPath (AccountsPrefix : acc : path)

-- | Get a state value at the given path.
getPath :: MonadIO m => STree.Path -> NodeT tx i m (Maybe STree.Val)
getPath k = do
    Handle{hStateTree} <- ask
    lift $ STree.get hStateTree k
