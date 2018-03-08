module Crypto.Data.Auth.Tree.Internal where

import           Prelude

import           Crypto.Hash (HashAlgorithm, Digest, hashUpdate, hashFinalize, hashInit, hashUpdates, digestFromByteString, hashDigestSize)
import           Data.ByteArray (ByteArrayAccess, zero)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe (fromJust)

hashLeaf
    :: (HashAlgorithm a, ByteArrayAccess k, ByteArrayAccess v)
    => k -> v -> Digest a
hashLeaf k v =
    hashFinalize $ flip hashUpdate v
                 $ flip hashUpdate k
                 $ flip hashUpdate (BS.singleton 0)
                 $ hashInit
hashNode
    :: HashAlgorithm a
    => Digest a -> Digest a -> Digest a
hashNode l r =
    hashFinalize $ flip hashUpdates [l, r]
                 $ flip hashUpdate (BS.singleton 1)
                 $ hashInit

emptyHash :: forall a. HashAlgorithm a => Digest a
emptyHash =
    fromJust $ digestFromByteString (zero n :: ByteString)
  where
    n = hashDigestSize (undefined :: a)

