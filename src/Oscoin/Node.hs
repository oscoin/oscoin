module Oscoin.Node
    ( Config (..)
    , Handle
    , NodeT

    , withNode
    , defaultEval

    , runNodeT

    , miner
    , storage

    , getMempool
    , getPath
    , getBestChain
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Consensus)
import qualified Oscoin.Consensus as Consensus
import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Consensus.BlockStore.Class
                 (MonadBlockStore(..), chainState, maximumChainBy)
import           Oscoin.Consensus.Class
                 (MonadClock(..), MonadQuery(..), MonadUpdate(..))
import           Oscoin.Consensus.Evaluator (EvalError, Evaluator)
import qualified Oscoin.Consensus.Evaluator.Radicle as Eval
import           Oscoin.Crypto.Blockchain (Blockchain, height)
import           Oscoin.Crypto.Blockchain.Block (prettyBlock)
import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.Data.Query
import           Oscoin.Data.Tx (Tx, toProgram)
import           Oscoin.Environment
import qualified Oscoin.Logging as Log
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.P2P as P2P
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import qualified Oscoin.Storage.Block as BlockStore

import qualified Radicle as Rad

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Data.Text.Prettyprint.Doc

-- | Node static config.
data Config = Config
    { cfgEnv    :: Environment
    , cfgLogger :: Log.Logger
    }

-- | Node handle.
data Handle tx s i = Handle
    { hConfig     :: Config
    , hNodeId     :: i
    , hStateTree  :: STree.Handle s
    , hBlockStore :: BlockStore.Handle tx s
    , hMempool    :: Mempool.Handle tx
    , hEval       :: Evaluator s tx ()
    , hConsensus  :: Consensus tx (NodeT tx s i IO)
    }

withNode
    :: (Hashable tx, Pretty tx)
    => Config
    -> i
    -> Mempool.Handle tx
    -> STree.Handle s
    -> BlockStore.Handle tx s
    -> Evaluator s tx ()
    -> Consensus tx (NodeT tx s i IO)
    -> (Handle tx s i -> IO c)
    -> IO c
withNode hConfig hNodeId hMempool hStateTree hBlockStore hEval hConsensus =
    bracket open close
  where
    open = do
        gen <- atomically $ BlockStore.for hBlockStore $ \bs ->
            BlockStore.getGenesisBlock bs
        Log.debug (cfgLogger hConfig) Log.stext (prettyBlock gen (Just 0))
        pure Handle{..}

    close = const $ pure ()

defaultEval :: Tx Rad.Value -> Eval.Env -> Either [EvalError] ((), Eval.Env)
defaultEval tx st = Eval.radicleEval (toProgram tx) st

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

miner
    :: ( MonadIO            m
       , P2P.MonadBroadcast m
       , MonadClock         m
       , Serialise tx
       , Hashable  tx
       , Ord       tx
       )
    => NodeT tx s i m a
miner = do
    Handle{hEval, hConsensus} <- ask
    forever $ do
        blk <-
            hoist liftIO
                . Consensus.mineBlock hConsensus hEval =<< currentTick
        for_ blk $ \blk' -> do
            lift . P2P.broadcast $ P2P.BlockMsg (void blk')
            updateChainState

storage
    :: ( MonadIO m
       , Hashable  tx
       , Ord       tx
       )
    => Storage tx (NodeT tx s i m)
storage = Storage
    { storageApplyBlock  = applyBlock
    , storageApplyTx     = applyTx
    , storageLookupBlock = (map . map) void . Storage.lookupBlock
    , storageLookupTx    = Storage.lookupTx
    }
  where
    applyBlock blk = do
        eval <- asks hEval
        res  <- Storage.applyBlock eval blk
        res <$ when (res == Storage.Applied) updateChainState

    applyTx tx = do
        res <- Storage.applyTx tx
        res <$ when (res == Storage.Applied) updateChainState

getMempool :: MonadIO m => NodeT tx s i m (Mempool tx)
getMempool = asks hMempool >>= liftIO . atomically . Mempool.snapshot

-- | Get a state value at the given path.
getPath :: (Query s, MonadIO m) => STree.Path -> NodeT tx s i m (Maybe (QueryVal s))
getPath = queryM

getBestChain :: (Hashable tx, Ord tx, MonadIO m) => NodeT tx s i m (Blockchain tx s)
getBestChain = maximumChainBy (comparing height)

-- Internal --------------------------------------------------------------------

withBlockStore
    :: MonadIO m
    => (BlockStore.BlockStore tx s -> b)
    -> NodeT tx s i m b
withBlockStore f = do
    bs <- asks hBlockStore
    liftIO . atomically $
        BlockStore.for bs f

updateChainState :: (MonadIO m, Hashable tx, Ord tx) => NodeT tx s i m ()
updateChainState = updateM =<< chainState . Consensus.cScore =<< asks hConsensus
