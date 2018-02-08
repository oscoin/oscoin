module Oscoin.Storage.Transaction where

import           Oscoin.Prelude
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Foldable (toList)
import qualified Data.Aeson as Aeson
import           Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar)

newtype Handle k tx = Handle { fromHandle :: TVar (Mempool k tx) }

newtype Mempool k tx = Mempool { fromMempool :: Map k tx }
    deriving (Show, Monoid, Eq)

instance Aeson.ToJSON tx => Aeson.ToJSON (Mempool k tx) where
    toJSON (Mempool txs) =
        Aeson.toJSON [Aeson.toJSON v | (_, v) <- Map.toList txs]

new :: (Monad m, MonadSTM m, Ord k) => m (Handle k tx)
new = do
    mp <- liftSTM $ newTVar mempty
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
    :: ( Has (Handle k tx) r
       , MonadReader r m
       , MonadSTM m )
    => (Mempool k tx -> Mempool k tx)
    -> m ()
updateMempool f = do
    Handle tvar <- asks getter
    liftSTM $ modifyTVar tvar f
