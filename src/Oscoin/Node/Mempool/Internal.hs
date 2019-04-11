{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Node.Mempool.Internal
    ( -- * Mempool
      Mempool
    , lookup
    , member
    , insert
    , insertMany
    , removeTxs
    , size
    , elems
    , toList
    ) where

import           Oscoin.Prelude hiding (toList)
import qualified Oscoin.Prelude as Prelude

import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Crypto.Hash as Crypto

import           Codec.Serialise (Serialise(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Mempool --------------------------------------------------------------------

-- | A map of transaction keys to transactions.
newtype Mempool c tx = Mempool (Map (Crypto.Hashed c tx) tx)
    deriving (Generic)

deriving instance (Show (Hash c), Show tx) => Show (Mempool c tx)
deriving instance (Eq (Hash c), Eq tx)  => Eq (Mempool c tx)
deriving instance Ord (Hash c) => Semigroup (Mempool c tx)
deriving instance Ord (Hash c) => Monoid (Mempool c tx)

instance (Ord (Hash c), Serialise tx, Serialise (Hash c)) => Serialise (Mempool c tx) where
    encode (Mempool txs) = encode (Map.elems txs)
    decode               = Mempool <$> decode

-- | Lookup a transaction in a mempool.
lookup :: Ord (Hash c) => Crypto.Hashed c tx -> Mempool c tx -> Maybe tx
lookup h (Mempool txs) = Map.lookup h txs

-- | Check for key membership.
member :: Ord (Hash c) => Crypto.Hashed c tx -> Mempool c tx -> Bool
member h (Mempool txs) = Map.member h txs

-- | Add a transaction to a mempool.
insert
    :: Crypto.Hashable c tx
    => tx
    -> Mempool c tx
    -> Mempool c tx
insert tx (Mempool txs) =
    Mempool (Map.insert (Crypto.hash tx) tx txs)

-- | Add multiple transactions to a mempool.
insertMany
    :: (Foldable t, Crypto.Hashable c tx)
    => t tx
    -> Mempool c tx
    -> Mempool c tx
insertMany txs mem = foldl' (flip insert) mem txs

-- | Remove multiple transactions from a mempool.
removeTxs
    :: (Foldable t, Crypto.Hashable c tx)
    => t tx -> Mempool c tx -> Mempool c tx
removeTxs ks (Mempool txs) =
    Mempool $ Map.withoutKeys txs keys
  where
    keys = Set.fromList . map Crypto.hash $ Prelude.toList ks

size :: Mempool c tx -> Int
size (Mempool txs) = Map.size txs

elems :: Mempool c tx -> [tx]
elems (Mempool txs) = Map.elems txs

toList :: Mempool c tx -> [(Crypto.Hashed c tx, tx)]
toList (Mempool txs) = Map.toList txs
