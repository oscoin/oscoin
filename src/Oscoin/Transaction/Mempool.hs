module Oscoin.Transaction.Mempool where
-- TODO: This module should probably be under Node.

import           Oscoin.Prelude
import           Oscoin.Node.Channel
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import           Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar, readTVar)

newtype Handle tx = Handle
    { fromHandle :: TVar (Evented tx (Mempool (Id tx) tx)) }

data Evented ev prod = Evented
    { evProducer    :: prod
    , evSubscribers :: Map (Id (Channel ev)) (Channel ev)
    }

evented :: Ord (Id (Channel ev)) => prod -> Evented ev prod
evented prod = Evented prod mempty

newtype Mempool k tx = Mempool { fromMempool :: Map k tx }
    deriving (Show, Semigroup, Monoid, Eq)

instance (Aeson.ToJSON k, Aeson.ToJSON tx) => Aeson.ToJSON (Mempool k tx) where
    toJSON (Mempool txs) =
        Aeson.toJSON [addId (Aeson.toJSON k) (Aeson.toJSON v) | (k, v) <- Map.toList txs]
      where
        addId k (Aeson.Object hm) = Aeson.Object $ HashMap.insert "id" k hm
        addId _ _                 = error "Unexpected value encountered"

type MonadMempool tx r m = (Has (Handle tx) r, MonadReader r m)

new :: (Ord (Id tx), Ord (Id (Channel tx))) => (Monad m, MonadSTM m) => m (Handle tx)
new = do
    mp <- liftSTM $ newTVar $ evented mempty
    pure $ Handle  mp

-- TODO: How can we make this type:
--
--      Hashable tx => tx -> Mempool k tx -> Mempool k tx
--
-- Or perhaps we need a `Hashable tx k`?
--
addTx :: Ord k => k -> tx -> Mempool k tx -> Mempool k tx
addTx k tx (Mempool txs) =
    Mempool (Map.insert k tx txs)

addTxs :: (Ord k, Foldable t) => t (k, tx) -> Mempool k tx -> Mempool k tx
addTxs txs' (Mempool txs) =
    Mempool $ Map.union txs (Map.fromList (toList txs'))

removeTxs :: (Ord k, Foldable t) => t k -> Mempool k tx -> Mempool k tx
removeTxs ks (Mempool txs) =
    Mempool $ Map.withoutKeys txs keys
  where
    keys = Set.fromList $ toList ks

lookup :: Ord k => k -> Mempool k tx -> Maybe tx
lookup k (Mempool txs) = Map.lookup k txs

updateMempool
    :: ( MonadMempool tx r m
       , MonadSTM m )
    => (Mempool (Id tx) tx -> Mempool (Id tx) tx)
    -> m ()
updateMempool f = do
    Handle tvar <- asks getter
    liftSTM $ modifyTVar tvar $ withProducer f

read :: (MonadMempool tx r m, MonadSTM m) => m (Mempool (Id tx) tx)
read = do
    Handle tvar <- asks getter
    evProducer <$> liftSTM (readTVar tvar)

-------------------------------------------------------------------------------

subscribe :: (MonadSTM m, MonadMempool tx r m) => Id (Channel tx) -> m (Channel tx)
subscribe key = do
    chan <- liftSTM newChannel
    addSubscriber key chan
    pure chan

addSubscriber :: (Ord (Id (Channel tx)), MonadSTM m, MonadMempool tx r m) => Id (Channel tx) -> Channel tx -> m ()
addSubscriber key chan = do
    Handle tvar <- asks getter
    liftSTM . modifyTVar tvar . withSubscribers $ Map.insert key chan

withProducer
    :: (Mempool (Id tx) tx -> Mempool (Id tx) tx)
    -> Evented tx (Mempool (Id tx) tx)
    -> Evented tx (Mempool (Id tx) tx)
withProducer = undefined

withSubscribers
    :: ( Map (Id (Channel ev)) (Channel ev)
      -> Map (Id (Channel ev)) (Channel ev))
    -> Evented ev prod
    -> Evented ev prod
withSubscribers f (Evented p subs) =
    Evented p (f subs)

