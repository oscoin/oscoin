module Oscoin.Node.State where

import           Oscoin.Prelude
import           Oscoin.Org (OrgId, OrgKey, OrgVal)
import qualified Oscoin.Storage.State as State

-- | Node state handle for interacting with the state tree.
data Handle = Handle
    { hStateTree :: State.Handle }

-- | The StorageT monad transformer.
type StorageT m a = ReaderT Handle m a

-- | Connect to state storage.
connect :: () -> IO Handle
connect () = do
    hStateTree <- State.connect
    pure Handle{..}

-- | Close the connection to state storage.
close :: Handle -> IO ()
close = notImplemented

-- | Set an org key to the given value.
setOrgKey :: Monad m => OrgId -> OrgKey -> OrgVal -> StorageT m ()
setOrgKey = notImplemented

-- | Run a storage action with the given state handle in another monad.
runStorageT :: Handle -> StorageT m a -> m a
runStorageT h s =
    runReaderT s h
