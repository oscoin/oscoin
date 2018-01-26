module Oscoin.Storage.State where

import           Oscoin.Prelude
import qualified Data.Map as Map
import           Data.IORef

type Key = Text
type Val = ByteString

-- | Database connection.
type Connection = ()

-- | Key/value tree data-structure.
type Tree k v = Map k v

-- | State tree handle.
data Handle = Handle
    { hTree :: IORef (Tree Key Val) -- ^ In-memory representation of the state tree.
    , hConn :: Connection           -- ^ Connection to database.
    }

connect :: IO Handle
connect = do
    ref <- newIORef mempty
    pure Handle
        { hTree = ref
        , hConn = ()
        }

close :: Handle -> IO ()
close _ = pass

lookup :: Handle -> Key -> IO (Maybe Val)
lookup Handle{hTree} k = do
    t <- readIORef hTree
    pure $ Map.lookup k t

set :: Handle -> Key -> Val -> IO ()
set Handle{hTree} k v =
    modifyIORef hTree (Map.insert k v)

