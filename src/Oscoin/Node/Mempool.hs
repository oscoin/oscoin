module Oscoin.Node.Mempool
    ( Mempool
    , Event (..)
    , Channel
    , Handle
    , new
    , newIO
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
new :: STM (Handle tx)
new = do
    hMempool   <- STM.newTVar mempty
    hBroadcast <- STM.newBroadcastTChan
    pure Handle{..}

-- | Like 'new', but without running a transaction.
newIO :: IO (Handle tx)
newIO = do
    hMempool   <- STM.newTVarIO mempty
    hBroadcast <- STM.newBroadcastTChanIO
    pure Handle{..}

insert :: Hashable tx => Handle tx -> tx -> STM ()
insert Handle{hMempool, hBroadcast} tx = do
    STM.modifyTVar' hMempool (Internal.insert tx)
    STM.writeTChan hBroadcast (Insert [tx])

insertMany :: (Hashable tx, Foldable t) => Handle tx -> t tx -> STM ()
insertMany Handle{hMempool, hBroadcast} txs = do
    STM.modifyTVar' hMempool (Internal.insertMany txs)
    STM.writeTChan hBroadcast (Insert (Fold.toList txs))

remove :: Hashable tx => Handle tx -> tx -> STM ()
remove Handle{hMempool, hBroadcast} tx = do
    STM.modifyTVar' hMempool (Internal.removeTxs [tx])
    STM.writeTChan hBroadcast (Remove [tx])

removeMany :: (Hashable tx, Foldable t) => Handle tx -> t tx -> STM ()
removeMany Handle{hMempool, hBroadcast} txs = do
    STM.modifyTVar' hMempool (Internal.removeTxs txs)
    STM.writeTChan hBroadcast (Remove (Fold.toList txs))

member :: Handle tx -> Hashed tx -> STM Bool
member hdl tx = Internal.member tx <$> snapshot hdl

lookup :: Handle tx -> Hashed tx -> STM (Maybe tx)
lookup hdl tx = Internal.lookup tx <$> snapshot hdl

size :: Handle tx -> STM Int
size hdl = Internal.size <$> snapshot hdl

toList :: Handle tx -> STM [(Hashed tx, tx)]
toList hdl = Internal.toList <$> snapshot hdl

subscribe :: Handle tx -> STM (TChan (Event tx))
subscribe = STM.dupTChan . hBroadcast

snapshot :: Handle tx -> STM (Mempool tx)
snapshot = STM.readTVar . hMempool
