module Oscoin.Node.State where

import           Oscoin.Prelude
import           Oscoin.Org (OrgId, OrgPath, OrgKey, OrgVal, mkOrgPath)
import           Oscoin.State.Tree (Val, Path)
import qualified Oscoin.Node.State.Mempool as Mempool
import           Oscoin.Node.State.Mempool (Mempool)
import qualified Oscoin.Node.State.Tree as STree
import qualified Oscoin.Storage.Block as BlockStore

-- | Node state handle for interacting with the state tree.
data Handle tx = Handle
    { hStateTree  :: STree.Handle
    , hBlockStore :: BlockStore.Handle
    , hMempool    :: Mempool.Handle tx
    }

instance Has (Mempool.Handle tx) (Handle tx) where
    getter = hMempool
    modifier = error "Read-only access allowed"

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

getMempool :: (Monad m, MonadSTM m) => StorageT tx m (Mempool (Id tx) tx)
getMempool = Mempool.read

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
