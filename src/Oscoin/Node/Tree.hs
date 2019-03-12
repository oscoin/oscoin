module Oscoin.Node.Tree
    ( Handle
    , Path
    , Key
    , Tree
    , Val
    , new
    , close
    , getPath
    , updateTree
    ) where

import           Oscoin.Data.Query
import           Oscoin.Prelude

import           Control.Concurrent.STM.TVar
import qualified Data.ByteString.Lazy as LBS

type Key = Text
type Path = [Text]
type Val = LBS.ByteString

-- | Key/value tree data-structure.
type Tree = Map Path Val


-- | State tree handle.
newtype Handle = Handle
    { hTree :: TVar Tree            -- ^ In-memory representation of the state tree.
    }

new :: MonadIO m => Tree -> m Handle
new tree = do
    ref <- liftIO $ newTVarIO tree
    pure Handle
        { hTree = ref
        }

close :: MonadIO m => Handle -> m ()
close _ = pass

getPath :: (MonadIO m) => Handle -> Path -> m (Maybe Val)
getPath Handle{hTree} k =
    query k <$> liftIO (readTVarIO hTree)

updateTree :: Handle -> Tree -> STM ()
updateTree = writeTVar . hTree
