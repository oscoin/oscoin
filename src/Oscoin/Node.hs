module Oscoin.Node
    ( Config (..)
    , Handle
    , NodeT

    , withNode
    , open
    , close
    , nodeEval

    , runNodeT

    , miner

    , getMempool
    , getPath
    , getBestChain

    , Receipt(..)
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
import           Oscoin.Crypto.Hash (Hashable, Hashed, toHex)
import           Oscoin.Data.Query
import           Oscoin.Data.Tx (Tx, toProgram)
import           Oscoin.Environment
import qualified Oscoin.Logging as Log
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.P2P as P2P
import qualified Oscoin.P2P.Gossip as Gossip
import qualified Oscoin.Storage.Block as BlockStore

import qualified Radicle as Rad

import           Codec.Serialise
import qualified Control.Exception.Safe as Safe (bracket)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson
                 ( FromJSON
                 , ToJSON
                 , object
                 , parseJSON
                 , toJSON
                 , withObject
                 , (.:)
                 , (.=)
                 )
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
    }

withNode
    :: (Hashable tx, Pretty tx)
    => Config
    -> i
    -> Mempool.Handle tx
    -> STree.Handle s
    -> BlockStore.Handle tx s
    -> (Handle tx s i -> IO c)
    -> IO c
withNode cfg i mem str blk = Safe.bracket (open cfg i mem str blk) close

open :: (Hashable tx, Pretty tx)
     => Config
     -> i
     -> Mempool.Handle tx
     -> STree.Handle s
     -> BlockStore.Handle tx s
     -> IO (Handle tx s i)
open hConfig hNodeId hMempool hStateTree hBlockStore = do
    gen <- atomically $ BlockStore.for hBlockStore $ \bs ->
        BlockStore.getGenesisBlock bs
    Log.debug (cfgLogger hConfig) Log.stext (prettyBlock gen (Just 0))
    pure Handle{..}

close :: Handle tx s i -> IO ()
close = const $ pure ()

nodeEval :: Tx Rad.Value -> Eval.Env -> Either [EvalError] ((), Eval.Env)
nodeEval tx st = Eval.radicleEval (toProgram tx) st

-------------------------------------------------------------------------------

newtype NodeT tx s i m a = NodeT (ReaderT (Handle tx s i) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Handle tx s i)
             , MonadTrans
             , MonadIO
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

withBlockStore
    :: (MonadReader (Handle tx s si) m, MonadIO m)
    => (BlockStore.BlockStore tx s -> b) -> m b
withBlockStore f = do
    bs <- asks hBlockStore
    liftIO . atomically $
        BlockStore.for bs f

miner
    :: ( Monad      m
       , MonadIO    m
       , MonadClock m
       , Serialise tx
       , Hashable  tx
       , Ord       tx
       )
    => Consensus tx (NodeT tx s i m)
    -> Evaluator s tx a
    -> Gossip.Handle e Gossip.Peer
    -> NodeT tx s i m b
miner consensus eval gossip = forever $ do
    blk <- Consensus.mineBlock consensus eval =<< lift currentTick
    for_ blk $ \blk' -> do
        liftIO $ P2P.broadcast gossip $ P2P.BlockMsg (void blk')
        updateM =<< chainState (Consensus.cScore consensus)

getMempool :: MonadIO m => NodeT tx s i m (Mempool tx)
getMempool = asks hMempool >>= liftIO . atomically . Mempool.snapshot

-- | Get a state value at the given path.
getPath :: (Query s, MonadIO m) => STree.Path -> NodeT tx s i m (Maybe (QueryVal s))
getPath = queryM

getBestChain :: (Hashable tx, Ord tx, MonadIO m) => NodeT tx s i m (Blockchain tx s)
getBestChain = maximumChainBy (comparing height)

-- | A transaction receipt. Contains the hashed transaction.
newtype Receipt tx = Receipt { fromReceipt :: Hashed tx }
    deriving (Show, Eq)

deriving instance Serialise (Receipt tx)

instance Hashable tx => ToJSON (Receipt tx) where
    toJSON (Receipt tx) =
        object [ "tx" .= decodeUtf8 (toHex tx) ]

instance Hashable tx => FromJSON (Receipt tx) where
    parseJSON = withObject "Receipt" $ \o ->
        Receipt <$> o .: "tx"
