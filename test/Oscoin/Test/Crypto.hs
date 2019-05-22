{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Crypto
    ( IsCrypto
    , Dict(..)
    , Hashable
    , HasHashing
    , upperBoundBytes
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto
import           Oscoin.Crypto.Address
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Hash.Mock
import           Oscoin.Crypto.PubKey
import           Oscoin.Data.Tx
import           Oscoin.Test.Util

import           Codec.Serialise (Serialise)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Data.Aeson
import           Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.Map as Map
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Orphans ()
import           Database.SQLite.Simple.ToField
import           Formatting.Buildable (Buildable)
import           Web.HttpApiData (FromHttpApiData(..))

import           Data.ByteArray.Hash

import           Test.QuickCheck (Arbitrary)

-- Ad-hoc instances for testing purposes

instance Semigroup (Hash MockCrypto) where
    (<>) (FnvHash (FnvHash64 a)) (FnvHash (FnvHash64 b)) =
        FnvHash $ FnvHash64 $ a + b -- Sums the `Word64` together.

instance Semigroup (Hash Crypto) where
    (<>) a b = hashByteArray @Crypto @ByteString $ on (<>) convert a b
    sconcat  = hashByteArray @Crypto @ByteString . foldMap convert

instance (HasHashing c, Semigroup (Hash c)) => Monoid (Hash c) where
    mempty = zeroHash

    mconcat []     = mempty
    mconcat (x:xs) = sconcat (x :| xs)


instance (Semigroup (Hash c), Hashable c a) => Hashable c [a] where
    hash = toHashed . fromHashed . foldMap hash

instance (Semigroup (Hash c), Hashable c a) => Hashable c (Seq a) where
    hash = toHashed . fromHashed . hash . toList

instance (Semigroup (Hash c), Hashable c k, Hashable c v) => Hashable c (Map k v) where
    hash = toHashed . fromHashed . hash . Map.toList

-- | A typeclass for estimating the serialised size in bytes of certain
-- data structures.
class Sized a where
    -- | The maximum (estimated) size in bytes.
    upperBoundBytes :: proxy a -> Int

instance Sized (Address Crypto) where
    upperBoundBytes _ = 47

instance Sized (Address MockCrypto) where
    upperBoundBytes _ = 31


-- | Kitchen-sink for all the constraints our cryptography must abide to.
type IsCrypto c = ( HasDigitalSignature c
                  , HasHashing c

                  , Arbitrary (Beneficiary c)
                  , ToJSON (Hash c)
                  , ToJSON (ShortHash c)
                  , ToJSON (PublicKey c)
                  , ToJSON (Signature c)
                  , FromJSON (PublicKey c)
                  , FromJSON (Signature c)
                  , FromJSON (ShortHash c)
                  , FromJSON (BlockHash c)
                  , FromHttpApiData (BlockHash c)
                  , Typeable c
                  , Serialise (PublicKey c)
                  , Serialise (Signature c)
                  , Serialise (BlockHash c)
                  , Serialise (ShortHash c)
                  , Ord (PublicKey c)
                  , Ord (Signature c)
                  , Ord (ShortHash c)
                  , Show (ShortHash c)
                  , Show (PublicKey c)
                  , Show (Signature c)
                  , Show (Hash c)
                  , Semigroup (Hash c)
                  , Condensed (PublicKey c)
                  , Condensed (PrivateKey c)
                  , Condensed (ShortHash c)
                  , Hashable c (Signed c Text)
                  , Hashable c [Word8]
                  , Hashable c Word8
                  , Hashable c Text
                  , Hashable c ByteString
                  , Hashable c LByteString
                  , Hashable c (PublicKey c)
                  , Hashable c (TxState c (Tx c))
                  , AuthTree.MerkleHash (Hash c)
                  , Ord (Hash c)
                  , ByteArrayAccess (BlockHash c)
                  , ByteArrayAccess (ShortHash c)
                  , ByteArrayAccess (PublicKey c)
                  , Buildable (Hash c)
                  , Buildable (ShortHash c)
                  , ToField (BlockHash c)
                  , ToField (Hashed c (Tx c))
                  , ToField (Sealed c Text)  -- DummySeal ~ Text
                  , FromField (Hash c)
                  , FromField (Sealed c Text)  -- DummySeal ~ Text
                  , Sized (Address c)
                  )
