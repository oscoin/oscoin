module Oscoin.Node.State where

import           Oscoin.Prelude
import           Oscoin.Org (OrgId, OrgPath, OrgKey, OrgVal, mkOrgPath)
import           Oscoin.State.Tree (Val, Path)
import qualified Oscoin.Storage.State as StateTree
import qualified Oscoin.Storage.Block as BlockStore
import qualified Oscoin.Storage.Transaction as Mempool

-- | Node state handle for interacting with the state tree.
data Handle = Handle
    { hStateTree  :: StateTree.Handle
    , hBlockStore :: BlockStore.Handle
    , hMempool    :: Mempool.Handle
    }

-- | The StorageT monad transformer.
type StorageT m a = ReaderT Handle m a

-- | Connect to state storage.
connect :: () -> IO Handle
connect () = do
    hStateTree <- StateTree.connect
    hBlockStore <- pure undefined
    hMempool <- pure undefined
    pure Handle{..}

-- | Close the connection to state storage.
close :: Handle -> IO ()
close = notImplemented

-- | Set an org path to the given value.
-- TODO: Shouldn't be a MonadIO, we need our own restricted class.
setOrgPath :: MonadIO m => OrgId -> OrgPath -> OrgVal -> StorageT m ()
setOrgPath org path =
    setPath (mkOrgPath org path)

setOrgKey :: MonadIO m => OrgId -> OrgKey -> OrgVal -> StorageT m ()
setOrgKey org key =
    setPath (mkOrgPath org [key])

getOrgPath :: MonadIO m => OrgId -> OrgPath -> StorageT m (Maybe OrgVal)
getOrgPath org path =
    getPath (mkOrgPath org path)

-- | Get a state value at the given path.
getPath :: MonadIO m => Path -> StorageT m (Maybe Val)
getPath k = do
    Handle{hStateTree} <- ask
    StateTree.get hStateTree k

-- | Set a state path to the given value.
setPath :: MonadIO m => Path -> Val -> StorageT m ()
setPath k v = do
    Handle{hStateTree} <- ask
    StateTree.set hStateTree k v

-- | Run a storage action with the given state handle in another monad.
runStorageT :: Handle -> StorageT m a -> m a
runStorageT h s =
    runReaderT s h
