module Oscoin.Node.State where

import           Oscoin.Prelude
import           Oscoin.Org (OrgId, OrgKey, OrgVal, mkOrgKey)
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
    hBlockStore <- notImplemented
    hMempool <- notImplemented
    pure Handle{..}

-- | Close the connection to state storage.
close :: Handle -> IO ()
close = notImplemented

-- | Set an org key to the given value.
-- TODO: Shouldn't be a MonadIO, we need our own restricted class.
setOrgKey :: MonadIO m => OrgId -> OrgKey -> OrgVal -> StorageT m ()
setOrgKey org key val = do
    Handle{hStateTree} <- ask
    StateTree.set hStateTree (mkOrgKey org key) val

-- | Run a storage action with the given state handle in another monad.
runStorageT :: Handle -> StorageT m a -> m a
runStorageT h s =
    runReaderT s h
