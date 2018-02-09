module Oscoin.Node.State where

import           Oscoin.Prelude
import           Oscoin.Org (OrgId, OrgPath, OrgKey, OrgVal, mkOrgPath)
import           Oscoin.State.Tree (Val, Path)
import qualified Oscoin.Storage.State as StateTree
import qualified Oscoin.Storage.Block as BlockStore
import qualified Oscoin.Storage.Transaction as Mempool
import           Oscoin.Storage.Transaction (Mempool)
import           Oscoin.Crypto.Hash (Hashed)

import           Control.Concurrent.STM.TVar (readTVar)

-- | Node state handle for interacting with the state tree.
data Handle k tx = Handle
    { hStateTree  :: StateTree.Handle
    , hBlockStore :: BlockStore.Handle
    , hMempool    :: Mempool.Handle (Hashed tx) tx
    }

instance Has (Mempool.Handle (Hashed tx) tx) (Handle k tx) where
    getter = hMempool
    modifier = error "Read-only access allowed"

-- | The StorageT monad transformer.
type StorageT tx m a = ReaderT (Handle (Hashed tx) tx) m a

-- | Connect to state storage.
connect :: Ord k => () -> IO (Handle k tx)
connect () = do
    hStateTree <- StateTree.connect
    hBlockStore <- pure undefined
    hMempool <- Mempool.new
    pure Handle{..}

-- | Close the connection to state storage.
close :: Handle k tx -> IO ()
close = notImplemented

getMempool :: (Monad m, MonadSTM m) => StorageT tx m (Mempool (Hashed tx) tx)
getMempool = do
    Handle{hMempool} <- ask
    liftSTM $ readTVar (Mempool.fromHandle hMempool)

-- TODO: Shouldn't be a MonadIO, we need our own restricted class.
-- | Set an org path to the given value.
setOrgPath :: MonadIO m => OrgId -> OrgPath -> OrgVal -> StorageT tx m ()
setOrgPath org path =
    setPath (mkOrgPath org path)

setOrgKey :: MonadIO m => OrgId -> OrgKey -> OrgVal -> StorageT tx m ()
setOrgKey org key =
    setPath (mkOrgPath org [key])

getOrgPath :: MonadIO m => OrgId -> OrgPath -> StorageT tx m (Maybe OrgVal)
getOrgPath org path =
    getPath (mkOrgPath org path)

-- | Get a state value at the given path.
getPath :: MonadIO m => Path -> StorageT tx m (Maybe Val)
getPath k = do
    Handle{hStateTree} <- ask
    StateTree.get hStateTree k

-- | Set a state path to the given value.
setPath :: MonadIO m => Path -> Val -> StorageT tx m ()
setPath k v = do
    Handle{hStateTree} <- ask
    StateTree.set hStateTree k v

-- | Run a storage action with the given state handle in another monad.
runStorageT :: Handle (Hashed tx) tx -> StorageT tx m a -> m a
runStorageT h s =
    runReaderT s h
