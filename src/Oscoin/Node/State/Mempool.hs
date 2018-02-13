module Oscoin.Node.State.Mempool
    ( -- * Mempool
      Mempool(..)
    , lookup
    , addTx
    , addTxs
    , removeTxs

      -- * MonadMempool
    , MonadMempool
    , Handle(..)
    , new
    , read
    , writeTxs
    , subscribe
    , unsubscribe

    , module Exports
    ) where

import           Oscoin.Prelude hiding (read, lookup)
import qualified Oscoin.Node.Channel as Exports (flushChannel, readChannel, Channel)
import           Oscoin.Node.Channel
import           Oscoin.Crypto.Hash (Hashed, Hashable, hash)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import           Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar, readTVar)

-- Mempool --------------------------------------------------------------------

-- | A map of transaction keys to transactions.
newtype Mempool k tx = Mempool { fromMempool :: Map k tx }
    deriving (Show, Semigroup, Monoid, Eq)

-- | The 'Aeson.ToJSON' instance of 'Mempool' includes transaction ids as fields
-- inside the transaction object.
instance (Aeson.ToJSON k, Aeson.ToJSON tx) => Aeson.ToJSON (Mempool k tx) where
    toJSON (Mempool txs) =
        Aeson.toJSON [addId (Aeson.toJSON k) (Aeson.toJSON v) | (k, v) <- Map.toList txs]
      where
        addId k (Aeson.Object hm) = Aeson.Object $ HashMap.insert "id" k hm
        addId _ _                 = error "Unexpected value encountered"

-- | Lookup a transaction in a mempool.
lookup :: Ord k => k -> Mempool k tx -> Maybe tx
lookup k (Mempool txs) = Map.lookup k txs

-- | Add a transaction to a mempool.
addTx
    :: (Hashable tx, Id tx ~ Hashed tx)
    => tx
    -> Mempool (Id tx) tx
    -> Mempool (Id tx) tx
addTx tx (Mempool txs) =
    Mempool (Map.insert (hash tx) tx txs)

-- | Add multiple transactions to a mempool.
addTxs
    :: (Foldable t, Hashable tx, Id tx ~ Hashed tx)
    => t tx
    -> Mempool (Id tx) tx
    -> Mempool (Id tx) tx
addTxs txs' (Mempool txs) =
    Mempool . Map.union txs
            . Map.fromList
            . map (\tx -> (hash tx, tx))
            $ toList txs'

-- | Remove multiple transactions from a mempool.
removeTxs :: (Ord k, Foldable t) => t k -> Mempool k tx -> Mempool k tx
removeTxs ks (Mempool txs) =
    Mempool $ Map.withoutKeys txs keys
  where
    keys = Set.fromList $ toList ks

-- MonadMempool ---------------------------------------------------------------

-- | Represents any monad which has a 'Handle' to a mempool in its environment.
type MonadMempool tx r m = (Has (Handle tx) r, MonadReader r m)

-- | Handle to a mutable 'Evented' 'Mempool'.
newtype Handle tx = Handle
    { fromHandle :: TVar (Evented tx (Mempool (Id tx) tx)) }

-- | Create a new 'Handle' with an underlying empty mempool.
new
    :: (Ord (Id tx), Monad m, MonadSTM m)
    => m (Handle tx)
new =
    Handle <$> (liftSTM $ newTVar $ evented mempty)

-- | Write transactions to the mempool, notifying all subscribers.
writeTxs
    :: (MonadMempool tx r m, MonadSTM m, Foldable t)
    => (Id tx ~ Hashed tx, Hashable tx)
    => t tx
    -> m ()
writeTxs txs = do
    update (addTxs txs)
    tvar <- fromHandle <$> asks getter
    evt  <- liftSTM (readTVar tvar)
    for_ txs $ notifySubscribers evt

-- | Return a read-only version of the mempool.
read :: (MonadMempool tx r m, MonadSTM m) => m (Mempool (Id tx) tx)
read = do
    Handle tvar <- asks getter
    evProducer <$> liftSTM (readTVar tvar)

-- | Subscribe to mempool events.
subscribe
    :: (MonadSTM m, MonadMempool tx r m)
    => Id (Channel tx) -- ^ Subscription key, used with 'unsubscribe'.
    -> m (Channel tx)
subscribe key = do
    chan <- liftSTM newChannel
    addSubscriber key chan
    pure chan

-- | Unsubscribe from mempool events.
unsubscribe
    :: (MonadSTM m, MonadMempool tx r m)
    => Id (Channel tx) -- ^ Subscription key.
    -> m ()
unsubscribe key = do
    Handle tvar <- asks getter
    liftSTM . modifyTVar tvar . mapSubscribers $ Map.delete key

-- Internal functions ---------------------------------------------------------

-- | Add a subscriber to the mempool.
addSubscriber
    :: (MonadSTM m, MonadMempool tx r m)
    => Id (Channel tx)
    -> Channel tx -> m ()
addSubscriber key chan = do
    Handle tvar <- asks getter
    liftSTM . modifyTVar tvar . mapSubscribers $ Map.insert key chan

-- | Update the mempool with an update function.
update
    :: (MonadMempool tx r m, MonadSTM m)
    => (Mempool (Id tx) tx -> Mempool (Id tx) tx)
    -> m ()
update f = do
    Handle tvar <- asks getter
    liftSTM $ modifyTVar tvar $ mapProducer f
