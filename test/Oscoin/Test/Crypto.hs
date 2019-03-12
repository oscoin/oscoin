{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Crypto
    ( IsCrypto
    , Dict(..)
    , Hashable
    , HasHashing
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Hash.Mock
import           Oscoin.Crypto.PubKey
import           Oscoin.Test.Util

import           Codec.Serialise (Serialise)
import qualified Crypto.Data.Auth.Tree.Internal as AuthTree
import           Data.Aeson
import           Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.Map as Map
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Formatting.Buildable (Buildable)
import           Oscoin.API.Types
import qualified Oscoin.Data.RadicleTx as Rad
import           Web.HttpApiData (FromHttpApiData(..))

import           Data.ByteArray.Hash

-- Ad-hoc instances for testing purposes

instance Semigroup (Hash MockCrypto) where
    (<>) (FnvHash (FnvHash64 a)) (FnvHash (FnvHash64 b)) =
        FnvHash $ FnvHash64 $ a + b -- Sums the `Word64` together.

instance Semigroup (Hash Crypto) where
    (<>) a b = fromByteArray @Crypto @ByteString $ on (<>) convert a b
    sconcat  = fromByteArray @Crypto @ByteString . foldMap convert

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


-- | Kitchen-sink for all the constraints our cryptography must abide to.
type IsCrypto c = ( HasDigitalSignature c
                  , HasHashing c

                  , ToJSON (Hash c)
                  , ToJSON (PK c)
                  , ToJSON (Signature c)
                  , FromJSON (PK c)
                  , FromJSON (Signature c)
                  , FromJSON (BlockHash c)
                  , FromHttpApiData (BlockHash c)
                  , Typeable c
                  , Serialise (PK c)
                  , Serialise (Signature c)
                  , Serialise (BlockHash c)
                  , Eq (PK c)
                  , Eq (Signature c)
                  , Show (PK c)
                  , Show (Signature c)
                  , Show (Hash c)
                  , Semigroup (Hash c)
                  , Condensed (PK c)
                  , Condensed (SK c)
                  , Hashable c (Signed c Text)
                  , Hashable c [Word8]
                  , Hashable c Word8
                  , Hashable c Text
                  , Hashable c ByteString
                  , Hashable c LByteString
                  , Hashable c (PK c)
                  , Hashable c (Rad.Env c)
                  , AuthTree.MerkleHash (Hash c)
                  , Ord (Hash c)
                  , ByteArrayAccess (BlockHash c)
                  , Buildable (Hash c)
                  , ToField (BlockHash c)
                  , ToField (Hashed c (RadTx c))
                  , ToField (Sealed c Text)  -- DummySeal ~ Text
                  , FromField (Hash c)
                  , FromField (Sealed c Text)  -- DummySeal ~ Text
                  )
