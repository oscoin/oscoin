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

import qualified Oscoin.Crypto.Hash as Crypto

import           Codec.Serialise (Serialise(..))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Mempool --------------------------------------------------------------------

-- | A map of transaction keys to transactions.
newtype Mempool tx = Mempool (Map (Crypto.Hashed tx) tx)
    deriving (Show, Semigroup, Monoid, Eq, Generic)

instance Serialise tx => Serialise (Mempool tx) where
    encode (Mempool txs) = encode (Map.elems txs)
    decode               = Mempool <$> decode

instance Aeson.ToJSON tx => Aeson.ToJSON (Mempool tx) where
    toJSON (Mempool txs) = Aeson.toJSON $ Map.elems txs

-- | Lookup a transaction in a mempool.
lookup :: Crypto.Hashed tx -> Mempool tx -> Maybe tx
lookup h (Mempool txs) = Map.lookup h txs

-- | Check for key membership.
member :: Crypto.Hashed tx -> Mempool tx -> Bool
member h (Mempool txs) = Map.member h txs

-- | Add a transaction to a mempool.
insert :: Crypto.Hashable tx => tx -> Mempool tx -> Mempool tx
insert tx (Mempool txs) =
    Mempool (Map.insert (Crypto.hash tx) tx txs)

-- | Add multiple transactions to a mempool.
insertMany :: (Foldable t, Crypto.Hashable tx) => t tx -> Mempool tx -> Mempool tx
insertMany txs mem = foldl' (flip insert) mem txs

-- | Remove multiple transactions from a mempool.
removeTxs :: (Foldable t, Crypto.Hashable tx) => t tx -> Mempool tx -> Mempool tx
removeTxs ks (Mempool txs) =
    Mempool $ Map.withoutKeys txs keys
  where
    keys = Set.fromList . map Crypto.hash $ Prelude.toList ks

size :: Mempool tx -> Int
size (Mempool txs) = Map.size txs

elems :: Mempool tx -> [tx]
elems (Mempool txs) = Map.elems txs

toList :: Mempool tx -> [(Crypto.Hashed tx, tx)]
toList (Mempool txs) = Map.toList txs
