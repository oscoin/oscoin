module Oscoin.Node where

import           Oscoin.Node.Service
import qualified Oscoin.Node.State.Mempool as Mempool
import qualified Oscoin.Node.State.Tree as STree

import           Oscoin.Account (AccId, Account, pattern AccountsPrefix)
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Environment
import qualified Oscoin.HTTP as HTTP
import           Oscoin.Node.State.Mempool (Mempool)
import qualified Oscoin.P2P as P2P
import           Oscoin.Prelude
import           Oscoin.State.Tree (Path, Val)
import qualified Oscoin.Storage.Block as BlockStore

import           Control.Concurrent.Async
import qualified Network.Socket as NS

-- | Node static config.
data Config = Config
    { cfgServiceName :: NS.ServiceName
    , cfgPeers       :: [(NS.HostName, NS.ServiceName)]
    , cfgEnv         :: Environment
    , cfgAccounts    :: [(AccId, Account)]
    }

-- | Node handle.
data Handle tx = Handle
    { hStateTree  :: STree.Handle
    , hBlockStore :: BlockStore.Handle
    , hMempool    :: Mempool.Handle tx
    }

data State = State ()

type NodeT m a = ServiceT State Config m a

run :: Config -> NodeT IO ()
run Config{..} = do
    mp <- Mempool.new
    st <- lift STree.connect
    threads <- lift . traverse async $
        [ HTTP.run (HTTP.api cfgEnv) cfgAccounts (readStr cfgServiceName) mp st
        , P2P.run cfgEnv mp st
        , runReaderT (Consensus.run cfgEnv st) mp
        ]
    (_, _err) <- lift $ waitAny threads
    pass

-- | The StorageT monad transformer.
type StorageT tx m a = ReaderT (Handle tx) m a

-- | Connect to state storage.
open :: Mempool.Handle tx -> STree.Handle -> IO (Handle tx)
open hMempool hStateTree = do
    hBlockStore <- pure undefined
    pure Handle{..}

-- | Close the connection to state storage.
close :: Handle tx -> IO ()
close = notImplemented

instance Has (Mempool.Handle tx) (Handle tx) where
    getter   = hMempool
    modifier = error "Read-only access allowed"

getMempool :: (Monad m, MonadSTM m) => StorageT tx m (Mempool (Id tx) tx)
getMempool = Mempool.read

-- TODO: Shouldn't be a MonadIO, we need our own restricted class.
-- | Set an account path to the given value.
setAccountPath :: MonadIO m => AccId -> Path -> Val -> StorageT tx m ()
setAccountPath acc path =
    setPath (AccountsPrefix : acc : path)

getAccountPath :: MonadIO m => AccId -> Path -> StorageT tx m (Maybe Val)
getAccountPath acc path =
    getPath (AccountsPrefix : acc : path)

-- | Get a state value at the given path.
getPath :: MonadIO m => Path -> StorageT tx m (Maybe Val)
getPath k = do
    Handle{hStateTree} <- ask
    STree.get hStateTree k

-- | Set a state path to the given value.
setPath :: MonadIO m => Path -> Val -> StorageT tx m ()
setPath k v = do
    Handle{hStateTree} <- ask
    STree.set hStateTree k v

-- | Run a storage action with the given state handle in another monad.
runStorageT :: Handle tx -> StorageT tx m a -> m a
runStorageT h s =
    runReaderT s h
