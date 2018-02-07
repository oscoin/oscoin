module Oscoin.Storage.Transaction where

import           Oscoin.Prelude
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable (toList)
import qualified Data.Aeson as Aeson
import           Control.Concurrent.STM.TVar (TVar, modifyTVar)

newtype Handle tx = Handle { fromHandle :: TVar (Mempool tx) }

newtype Mempool tx = Mempool { fromMempool :: Set tx }
    deriving (Show, Monoid, Eq, Aeson.ToJSON)

addTx :: Ord tx => tx -> Mempool tx -> Mempool tx
addTx tx (Mempool txs) = Mempool (Set.insert tx txs)

addTxs :: (Ord tx, Foldable t) => t tx -> Mempool tx -> Mempool tx
addTxs txs' (Mempool txs) =
    Mempool $ Set.union txs (Set.fromList (toList txs'))

removeTxs :: (Ord tx, Foldable t) => t tx -> Mempool tx -> Mempool tx
removeTxs txs' (Mempool txs) =
    Mempool $ Set.difference (Set.fromList (toList txs')) txs

updateMempool
    :: ( Has (Handle tx) r
       , MonadReader r m
       , MonadSTM m )
    => (Mempool tx -> Mempool tx)
    -> m ()
updateMempool f = do
    Handle tvar <- asks getter
    liftSTM $ modifyTVar tvar f
