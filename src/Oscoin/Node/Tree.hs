module Oscoin.Node.Tree
    ( Handle
    , Path
    , new
    , close
    , getPath
    , updateTree
    ) where

import           Oscoin.Prelude
import           Oscoin.State.Tree (Path)
import           Oscoin.Data.Query

import           Control.Concurrent.STM.TVar

-- | Database connection.
type Connection = ()

-- | State tree handle.
data Handle s = Handle
    { hTree :: TVar s            -- ^ In-memory representation of the state tree.
    , hConn :: Connection        -- ^ Connection to database.
    }

new :: (MonadIO m, Default s) => m (Handle s)
new = do
    ref <- io $ newTVarIO def
    pure Handle
        { hTree = ref
        , hConn = ()
        }

close :: MonadIO m => Handle s -> m ()
close _ = pass

getPath :: (Query s, MonadIO m) => Handle s -> Path -> m (Maybe (QueryVal s))
getPath Handle{hTree} k = do
    t <- io $ readTVarIO hTree
    pure $ query k t

updateTree :: Handle s -> s -> STM ()
updateTree Handle{hTree} tree =
    writeTVar hTree tree
