module Oscoin.Node.State where

import           Oscoin.Prelude
import           Oscoin.Org (OrgId, OrgKey, OrgVal)

newtype Handle = Handle ()

type StorageT m a = ReaderT Handle m a

connect :: () -> IO Handle
connect = notImplemented

close :: Handle -> IO ()
close = notImplemented

setOrgKey :: Monad m => OrgId -> OrgKey -> OrgVal -> StorageT m ()
setOrgKey = notImplemented

runStorageT :: Handle -> StorageT m a -> m a
runStorageT h s =
    runReaderT s h
