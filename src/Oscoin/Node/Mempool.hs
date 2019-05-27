module Oscoin.Node.Mempool
    ( Mempool
    , Event (..)
    , Channel
    , Handle
    , new
    , newIO
    , insert
    , remove
    , removeMany
    , member
    , lookup
    , size
    , toList
    , subscribe
    , snapshot
    ) where

import           Oscoin.Prelude hiding (toList)

import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.Tx.Abstract
import           Oscoin.Node.Mempool.Event (Channel, Event(..))
import           Oscoin.Node.Mempool.Internal (Mempool)
import qualified Oscoin.Node.Mempool.Internal as Internal

import           Control.Concurrent.STM (TChan, TVar)
import qualified Control.Concurrent.STM as STM
import qualified Data.Foldable as Fold

-- | Handle to a mutable 'Mempool'.
data Handle c tx = Handle
    { hMempool    :: TVar (Mempool c tx)
    , hBroadcast  :: Channel tx
    , hTxValidate :: tx -> Either (TxValidationError c tx) ()
    }

-- | Create a new 'Handle' with an underlying empty mempool.
new :: Ord (Hash c) => TxValidator c tx -> STM (Handle c tx)
new hTxValidate = do
    hMempool   <- STM.newTVar mempty
    hBroadcast <- STM.newBroadcastTChan
    pure Handle{..}

-- | Like 'new', but without running a transaction.
newIO :: Ord (Hash c) => TxValidator c tx -> IO (Handle c tx)
newIO hTxValidate = do
    hMempool   <- STM.newTVarIO mempty
    hBroadcast <- STM.newBroadcastTChanIO
    pure Handle{..}

insert
    :: ( Crypto.Hashable c tx
       )
    => Handle c tx
    -> tx
    -> STM (Either (TxValidationError c tx) ())
insert Handle{hMempool, hBroadcast, hTxValidate} tx =
    case hTxValidate tx of
        Left err -> pure $ Left err
        Right () -> do
            STM.modifyTVar' hMempool (Internal.insert tx)
            STM.writeTChan hBroadcast (Insert [tx])
            pure $ Right ()


remove :: (Crypto.Hashable c tx) => Handle c tx -> tx -> STM ()
remove Handle{hMempool, hBroadcast} tx = do
    STM.modifyTVar' hMempool (Internal.removeTxs [tx])
    STM.writeTChan hBroadcast (Remove [tx])

removeMany
    :: ( Crypto.Hashable c tx
       , Foldable t
       )
    => Handle c tx
    -> t tx
    -> STM ()
removeMany Handle{hMempool, hBroadcast} txs = do
    STM.modifyTVar' hMempool (Internal.removeTxs txs)
    STM.writeTChan hBroadcast (Remove (Fold.toList txs))

member :: Ord (Hash c) => Handle c tx -> Crypto.Hashed c tx -> STM Bool
member hdl tx = Internal.member tx <$> snapshot hdl

lookup :: Ord (Hash c) => Handle c tx -> Crypto.Hashed c tx -> STM (Maybe tx)
lookup hdl tx = Internal.lookup tx <$> snapshot hdl

size :: Handle c tx -> STM Int
size hdl = Internal.size <$> snapshot hdl

toList :: Handle c tx -> STM [(Crypto.Hashed c tx, tx)]
toList hdl = Internal.toList <$> snapshot hdl

subscribe :: Handle c tx -> STM (TChan (Event tx))
subscribe = STM.dupTChan . hBroadcast

snapshot :: Handle c tx -> STM (Mempool c tx)
snapshot = STM.readTVar . hMempool
