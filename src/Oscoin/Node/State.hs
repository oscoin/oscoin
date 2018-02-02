module Oscoin.Node.State where

import           Oscoin.Prelude
import           Oscoin.Org (OrgId, OrgKey, OrgVal, mkOrgKey)
import qualified Oscoin.Storage.State as StateTree
import           Oscoin.Storage.State (Key, Val)
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

-- | Set an org key to the given value.
-- TODO: Shouldn't be a MonadIO, we need our own restricted class.
setOrgKey :: MonadIO m => OrgId -> OrgKey -> OrgVal -> StorageT m ()
setOrgKey org key =
    setKey (mkOrgKey org key)

getOrgKey :: MonadIO m => OrgId -> OrgKey -> StorageT m (Maybe OrgVal)
getOrgKey org key =
    getKey (mkOrgKey org key)

-- | Get a state value at the given key.
getKey :: MonadIO m => Key -> StorageT m (Maybe Val)
getKey k = do
    Handle{hStateTree} <- ask
    StateTree.get hStateTree k

-- | Set a state key to the given value.
setKey :: MonadIO m => Key -> Val -> StorageT m ()
setKey k v = do
    Handle{hStateTree} <- ask
    StateTree.set hStateTree k v

-- | Run a storage action with the given state handle in another monad.
runStorageT :: Handle -> StorageT m a -> m a
runStorageT h s =
    runReaderT s h
