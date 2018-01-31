module Oscoin.Storage.State where

import           Oscoin.Prelude
import qualified Data.Map as Map
import           Data.IORef

type Key = [Text]
type Val = LByteString

-- | Database connection.
type Connection = ()

-- | Key/value tree data-structure.
type Tree k v = Map k v

-- | State tree handle.
data Handle = Handle
    { hTree :: IORef (Tree Key Val) -- ^ In-memory representation of the state tree.
    , hConn :: Connection           -- ^ Connection to database.
    }

connect :: MonadIO m => m Handle
connect = do
    ref <- io $ newIORef mempty
    pure Handle
        { hTree = ref
        , hConn = ()
        }

close :: MonadIO m => Handle -> m ()
close _ = pass

lookup :: MonadIO m => Handle -> Key -> m (Maybe Val)
lookup Handle{hTree} k = do
    t <- io $ readIORef hTree
    pure $ Map.lookup k t

set :: MonadIO m => Handle -> Key -> Val -> m ()
set Handle{hTree} k v =
    io $ modifyIORef hTree (Map.insert k v)

