module Oscoin.Node.Tree
    ( Handle
    , Path
    , new
    , close
    , getPath
    , updateTree
    ) where

import           Oscoin.Data.Query
import           Oscoin.Prelude
import           Oscoin.State.Tree (Path)

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
getPath Handle{hTree} k =
    query k <$> io (readTVarIO hTree)

updateTree :: Handle s -> s -> STM ()
updateTree = writeTVar . hTree
