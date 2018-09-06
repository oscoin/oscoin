module Oscoin.Node
    ( Config (..)
    , Handle
    , NodeT

    , withNode
    , open
    , close
    , nodeEval

    , runNodeT
    , runEffects

    , step
    , tick

    , getMempool
    , getPath

    , Receipt(..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..), maximumChainBy)
import           Oscoin.Consensus.Class (MonadClock(..), MonadProtocol(..), MonadQuery(..))
import qualified Oscoin.Consensus.Evaluator.Radicle as Eval
import           Oscoin.Consensus.Evaluator (EvalError)
import           Oscoin.Crypto.Hash (Hashable, Hashed, toHex)
import           Oscoin.Crypto.Blockchain (tip, height, blockState, blockHeader)
import           Oscoin.Crypto.Blockchain.Block (prettyBlock)
import           Oscoin.Data.Tx (Tx, toProgram)
import           Oscoin.Data.Query
import           Oscoin.Environment
import qualified Oscoin.Logging as Log
import           Oscoin.Logging ((%))
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Tree as STree
import           Oscoin.P2P (MonadNetwork(..), Msg(..), runNetworkT)
import qualified Oscoin.P2P as P2P
import qualified Oscoin.Storage.Block as BlockStore

import qualified Radicle as Rad

import           Codec.Serialise
import           Control.Exception.Safe (bracket)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, toJSON, object, (.=), (.:))
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

-- | Node static config.
data Config = Config
    { cfgEnv         :: Environment
    , cfgLogger      :: Log.Logger
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
withNode cfg i mem str blk = bracket (open cfg i mem str blk) close

-- | Connect to state storage.
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
    Log.debug (cfgLogger hConfig) ("" % Log.string) (prettyBlock gen (Just 0))
    pure Handle{..}

-- | Close the connection to state storage.
close :: Handle tx s i -> IO ()
close = const $ pure ()

tick :: forall r tx m.
        ( MonadNetwork     tx m
        , MonadProtocol    tx m
        , MonadClock          m
        , Log.MonadLogger r   m
        , Hashable         tx
        , Pretty           tx
        )
     => m ()
tick = do
    msgs <- tickM =<< currentTick
    forM_ msgs logMsg
    sendM msgs


logMsg :: forall r tx m.
    ( Hashable tx, Pretty tx, Log.MonadLogger r m )
    => Msg tx -> m ()
logMsg msg = Log.debugM Log.string (prettyMsg msg)
  where
    prettyMsg :: Msg tx -> String
    prettyMsg (BlockMsg blk)  = prettyBlock blk Nothing
    prettyMsg (TxMsg    tx)   = T.unpack . renderStrict . layoutCompact . pretty $ tx
    prettyMsg (ReqBlockMsg h) = show h


step :: forall            r tx          m.
        ( MonadNetwork      tx          m
        , MonadProtocol     tx          m
        , MonadBlockStore   tx Eval.Env m
        , MonadClock                    m
        , Log.MonadLogger r             m
        )
     => m ()
step = do
    r <- recvM
    t <- currentTick
    o <- stepM t r
    sendM o

    st <- Rad.bindingsEnv . Eval.fromEnv . blockState . blockHeader . tip
      <$> maximumChainBy (comparing height)
    Log.debugM ("State: " % Log.shown) st

nodeEval :: Tx Rad.Value -> Eval.Env -> Either [EvalError] ((), Eval.Env)
nodeEval tx st = Eval.radicleEval (toProgram tx) st

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
    -> (cfg -> NodeT tx s i (P2P.NetworkT tx m) a)
    -> cfg
    -> m a
runEffects p2p node evalConsensusT =
    runNetworkT p2p . runNodeT node . evalConsensusT

instance (Hashable tx, Monad m, MonadIO m) => MonadMempool tx (NodeT tx s i m) where
    addTxs txs = asks hMempool >>= io . atomically . (`Mempool.insertMany` txs)
    getTxs     = asks hMempool >>= io . atomically . Mempool.toList
    delTxs txs = asks hMempool >>= io . atomically . (`Mempool.removeMany` txs)
    numTxs     = asks hMempool >>= io . atomically . Mempool.size
    lookupTx h = asks hMempool >>= io . atomically . (`Mempool.lookup` h)
    subscribe  = asks hMempool >>= io . atomically . Mempool.subscribe

    {-# INLINE addTxs    #-}
    {-# INLINE getTxs    #-}
    {-# INLINE delTxs    #-}
    {-# INLINE numTxs    #-}
    {-# INLINE subscribe #-}

instance (Monad m, MonadIO m, Ord tx, Hashable tx) => MonadBlockStore tx s (NodeT tx s i m) where
    storeBlock blk = do
        bs <- asks hBlockStore
        io . atomically $ BlockStore.put bs blk

    lookupBlock hdr = do
        bs <- asks hBlockStore
        io . atomically $
            BlockStore.for bs $
                BlockStore.lookupBlock hdr

    getGenesisBlock = do
        bs <- asks hBlockStore
        io . atomically $
            BlockStore.for bs $
                BlockStore.getGenesisBlock

    lookupTx tx = do
        bs <- asks hBlockStore
        io . atomically $
            BlockStore.for bs $
                BlockStore.lookupTx tx

    orphans = do
        bs <- asks hBlockStore
        io . atomically $
            BlockStore.for bs $
                BlockStore.orphans

    maximumChainBy cmp = do
        bs <- asks hBlockStore
        io . atomically $
            BlockStore.for bs $
                BlockStore.maximumChainBy cmp

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

instance MonadClock m => MonadClock (NodeT tx s i m) where
    currentTick = lift currentTick
    {-# INLINE currentTick #-}

instance MonadIO m => MonadIO (NodeT tx s i m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadNetwork tx m => MonadNetwork tx (NodeT tx s i m)

-------------------------------------------------------------------------------

getMempool :: MonadIO m => NodeT tx s i m (Mempool tx)
getMempool = asks hMempool >>= io . atomically . Mempool.snapshot

-- | Get a state value at the given path.
getPath :: (Query s, MonadIO m) => STree.Path -> NodeT tx s i m (Maybe (QueryVal s))
getPath = queryM

-- | A transaction receipt. Contains the hashed transaction.
newtype Receipt tx = Receipt { fromReceipt :: Hashed tx }
    deriving (Show)

deriving instance Serialise (Receipt tx)

instance Hashable tx => ToJSON (Receipt tx) where
    toJSON (Receipt tx) =
        object [ "tx" .= decodeUtf8 (toHex tx) ]

instance Hashable tx => FromJSON (Receipt tx) where
    parseJSON = withObject "Receipt" $ \o ->
        Receipt <$> o .: "tx"
