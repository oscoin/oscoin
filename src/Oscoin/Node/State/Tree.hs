module Oscoin.Node.State.Tree where

import           Oscoin.Prelude
import qualified Oscoin.State.Tree as Tree
import           Oscoin.State.Tree (Tree, Val, Path)
import           Data.IORef

-- | Database connection.
type Connection = ()

-- | State tree handle.
data Handle = Handle
    { hTree :: IORef (Tree Path Val) -- ^ In-memory representation of the state tree.
    , hConn :: Connection            -- ^ Connection to database.
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

get :: MonadIO m => Handle -> Path -> m (Maybe Val)
get Handle{hTree} k = do
    t <- io $ readIORef hTree
    pure $ Tree.get k t

set :: MonadIO m => Handle -> Path -> Val -> m ()
set Handle{hTree} k v =
    io $ modifyIORef hTree (Tree.set k v)

update :: MonadIO m => Handle -> (Tree Path Val -> Tree Path Val) -> m ()
update Handle{hTree} f =
    io $ modifyIORef hTree f
