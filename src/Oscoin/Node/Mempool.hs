module Oscoin.Node.Mempool
    ( Mempool
    , Event (..)
    , Channel
    , Handle
    , new
    , insert
    , insertMany
    , remove
    , removeMany
    , member
    , lookup
    , size
    , toList
    , subscribe
    , snapshot
    ) where

import           Oscoin.Prelude hiding (lookup, toList)

import           Oscoin.Crypto.Hash (Hashable, Hashed)
import           Oscoin.Node.Mempool.Event (Channel, Event(..))
import           Oscoin.Node.Mempool.Internal (Mempool)
import qualified Oscoin.Node.Mempool.Internal as Internal

import           Control.Concurrent.STM (TChan, TVar)
import qualified Control.Concurrent.STM as STM
import qualified Data.Foldable as Fold

-- | Handle to a mutable 'Mempool'.
data Handle tx = Handle
    { hMempool   :: TVar (Mempool tx)
    , hBroadcast :: Channel tx
    }

-- | Create a new 'Handle' with an underlying empty mempool.
new :: (Monad m, MonadSTM m) => m (Handle tx)
new = do
    hMempool   <- liftSTM (STM.newTVar mempty)
    hBroadcast <- liftSTM STM.newBroadcastTChan
    pure Handle{..}

insert :: (MonadSTM m, Hashable tx) => Handle tx -> tx -> m ()
insert Handle{hMempool, hBroadcast} tx = liftSTM $ do
    STM.modifyTVar' hMempool (Internal.insert tx)
    STM.writeTChan hBroadcast (Insert [tx])

insertMany :: (MonadSTM m, Hashable tx, Foldable t) => Handle tx -> t tx -> m ()
insertMany Handle{hMempool, hBroadcast} txs = liftSTM $ do
    STM.modifyTVar' hMempool (Internal.insertMany txs)
    STM.writeTChan hBroadcast (Insert (Fold.toList txs))

remove :: (MonadSTM m, Hashable tx) => Handle tx -> tx -> m ()
remove Handle{hMempool, hBroadcast} tx = liftSTM $ do
    STM.modifyTVar' hMempool (Internal.removeTxs [tx])
    STM.writeTChan hBroadcast (Remove [tx])

removeMany :: (MonadSTM m, Hashable tx, Foldable t) => Handle tx -> t tx -> m ()
removeMany Handle{hMempool, hBroadcast} txs = liftSTM $ do
    STM.modifyTVar' hMempool (Internal.removeTxs txs)
    STM.writeTChan hBroadcast (Remove (Fold.toList txs))

member :: (MonadSTM m, Functor m) => Handle tx -> Hashed tx -> m Bool
member hdl tx = Internal.member tx <$> snapshot hdl

lookup :: (MonadSTM m, Functor m) => Handle tx -> Hashed tx -> m (Maybe tx)
lookup hdl tx = Internal.lookup tx <$> snapshot hdl

size :: (MonadSTM m, Functor m) => Handle tx -> m Int
size hdl = Internal.size <$> snapshot hdl

toList :: (MonadSTM m, Functor m) => Handle tx -> m [(Hashed tx, tx)]
toList hdl = Internal.toList <$> snapshot hdl

subscribe :: MonadSTM m => Handle tx -> m (TChan (Event tx))
subscribe = liftSTM . STM.dupTChan . hBroadcast

snapshot :: MonadSTM m => Handle tx -> m (Mempool tx)
snapshot = liftSTM . STM.readTVar . hMempool
