module Oscoin.Node
    ( Config (..)
    , Handle
    , NodeT

    , withNode
    , open
    , close

    , runNodeT
    , runEffects

    , step
    , tick

    , getMempool
    , getAccountPath
    , getPath
    ) where

import           Oscoin.Prelude

import           Oscoin.Account (AccId, Account, pattern AccountsPrefix)
import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..), maximumChainBy)
import           Oscoin.Consensus.Class (MonadClock(..), MonadProtocol(..), MonadQuery(..))
import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.Crypto.Blockchain (tip, height, blockState, blockHeader)
import           Oscoin.Environment
import qualified Oscoin.Logging as Log
import           Oscoin.Logging ((%))
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Tree as STree
import           Oscoin.P2P (MonadNetwork(..), Msg, runNetworkT)
import qualified Oscoin.P2P as P2P
import qualified Oscoin.Storage.Block as BlockStore

import qualified Radicle as Rad

import           Control.Exception.Safe (bracket)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Network.Socket as NS

-- | Node static config.
data Config = Config
    { cfgServiceName :: NS.ServiceName
    , cfgPeers       :: [(NS.HostName, NS.ServiceName)]
    , cfgEnv         :: Environment
    , cfgAccounts    :: [(AccId, Account)]
    , cfgLogger      :: Log.Logger
    }

-- | Node handle.
data Handle tx s i = Handle
    { hConfig     :: Config
    , hNodeId     :: i
    , hStateTree  :: STree.Handle
    , hBlockStore :: BlockStore.Handle tx s
    , hMempool    :: Mempool.Handle tx
    }

withNode
    :: Config
    -> i
    -> Mempool.Handle tx
    -> STree.Handle
    -> BlockStore.Handle tx s
    -> (Handle tx s i -> IO c)
    -> IO c
withNode cfg i mem str blk = bracket (open cfg i mem str blk) close

-- | Connect to state storage.
open :: Config
     -> i
     -> Mempool.Handle tx
     -> STree.Handle
     -> BlockStore.Handle tx s
     -> IO (Handle tx s i)
open hConfig hNodeId hMempool hStateTree hBlockStore =
    pure Handle{..}

-- | Close the connection to state storage.
close :: Handle tx s i -> IO ()
close = const $ pure ()

tick :: forall proxy tx m.
        ( MonadNetwork  tx m
        , MonadProtocol tx m
        , MonadClock       m
        )
     => proxy tx
     -> m ()
tick _ =
    currentTick >>= tickM >>= sendM

step :: forall proxy    w tx s m r.
        ( MonadNetwork    tx   m
        , MonadProtocol   tx   m
        , MonadBlockStore tx s m
        , MonadClock           m
        , MonadReader     r    m
        , Has Log.Logger  r
        , Rad.Bindings   w ~ s
        , MonadIO              m
        )
     => proxy tx
     -> m ()
step _ = do
    l :: Log.Logger <- asks getter

    r <- recvM :: m (Msg tx)
    t <- currentTick
    o <- stepM t r
    sendM o

    st <- Rad.bindingsEnv . blockState . blockHeader . tip
      <$> maximumChainBy (comparing height)
    liftIO $ Log.info l ("State: " % Log.string) (show st)

-------------------------------------------------------------------------------

newtype NodeT tx s i m a = NodeT (ReaderT (Handle tx s i) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Handle tx s i)
             , MonadTrans
             )

runNodeT :: Handle tx s i -> NodeT tx s i m a -> m a
runNodeT env (NodeT ma) = runReaderT ma env

runEffects
    :: P2P.Handle
    -> Handle tx s i
    -> (c -> NodeT tx s i (P2P.NetworkT tx m) a)
    -> c
    -> m a
runEffects p2p node evalConsensusT =
    runNetworkT p2p . runNodeT node . evalConsensusT

instance (Hashable tx, Monad m, MonadSTM m) => MonadMempool tx (NodeT tx s i m) where
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

instance (Monad m, MonadSTM m, Ord tx, Hashable tx) => MonadBlockStore tx s (NodeT tx s i m) where
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

instance (Monad m, MonadIO m) => MonadQuery (NodeT tx s i m) where
    type Key (NodeT tx s i m) = STree.Path
    type Val (NodeT tx s i m) = STree.Val

    queryM k = do
        st <- asks hStateTree
        lift $ STree.get st k
    {-# INLINE queryM #-}

instance MonadClock m => MonadClock (NodeT tx s i m) where
    currentTick = lift currentTick
    {-# INLINE currentTick #-}

instance MonadIO m => MonadIO (NodeT tx s i m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadNetwork tx m => MonadNetwork tx (NodeT tx s i m)

-------------------------------------------------------------------------------

getMempool :: (Monad m, MonadSTM m) => NodeT tx s i m (Mempool tx)
getMempool = asks hMempool >>= Mempool.snapshot

getAccountPath :: MonadIO m => AccId -> STree.Path -> NodeT tx s i m (Maybe STree.Val)
getAccountPath acc path =
    getPath (AccountsPrefix : acc : path)

-- | Get a state value at the given path.
getPath :: MonadIO m => STree.Path -> NodeT tx s i m (Maybe STree.Val)
getPath k = do
    Handle{hStateTree} <- ask
    lift $ STree.get hStateTree k
