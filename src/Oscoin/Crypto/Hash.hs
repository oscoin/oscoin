{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module Oscoin.Crypto.Hash
    ( Hashed
    , ShortHashed
    , Hashable(..)
    , HasHashing(..)
    , toHashed
    , fromHashed
    , fromShortHashed

    , formatHash
    , formatHashed
    , formatShortHash

    , hashBinary
    , hashSerial

    -- * Temporary multihash shim
    , encodeAtBase
    , decodeAtBase
    ) where

import           Oscoin.Prelude

import           Codec.Serialise.Orphans ()

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serial
import           Crypto.Hash (Digest)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Data.Aeson (FromJSON(..), ToJSON(..))
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Lazy as LBS
import           Data.Multihash (Multihash, Multihashable)
import qualified Data.Multihash as Multihash
import qualified Database.SQLite.Simple.ToField as Sql
import           Formatting (Format)
import qualified Formatting as Fmt
import           Web.HttpApiData (ToHttpApiData(..))

{------------------------------------------------------------------------------
-- Little multihash shim needed while we tackle
-- https://github.com/oscoin/ipfs/issues/43
------------------------------------------------------------------------------}

-- | Decode a 'C.Digest' from a multihash- and base-n-encoded 'ByteString'.
decodeAtBase
    :: forall a b.
       ( Multihashable    a
       , BaseN.DecodeBase b
       )
    => BaseN.Base b
    -> ByteString
    -> Either String (Digest a)
decodeAtBase base = BaseN.decodeAtBaseEither base >=> Multihash.decodeDigest

-- | Encode a 'Multihash' at a 'BaseN.Base'.
encodeAtBase :: BaseN.Base b -> Multihash -> BaseN.AtBase b
encodeAtBase base mhash = BaseN.encodeAtBase base (Multihash.encodedBytes mhash)

-- | A 'Hash'' tagged by its pre-image
newtype Hashed      crypto a = Hashed      (Hash crypto)
newtype ShortHashed crypto a = ShortHashed (ShortHash crypto)

deriving instance Eq (Hash crypto) => Eq (Hashed crypto a)
deriving instance Ord (Hash crypto) => Ord (Hashed crypto a)
deriving instance Show (Hash crypto) => Show (Hashed crypto a)
deriving instance Semigroup (Hash crypto) => Semigroup (Hashed crypto a)
deriving instance Monoid (Hash crypto) => Monoid (Hashed crypto a)
deriving instance ByteArrayAccess (Hash crypto) => ByteArrayAccess (Hashed crypto a)
deriving instance Serialise (Hash crypto) => Serialise (Hashed crypto a)
deriving instance ToJSON (Hash crypto) => ToJSON (Hashed crypto a)
deriving instance FromJSON (Hash crypto) => FromJSON (Hashed crypto a)

deriving instance Eq (ShortHash crypto) => Eq (ShortHashed crypto a)
deriving instance Ord (ShortHash crypto) => Ord (ShortHashed crypto a)
deriving instance Show (ShortHash crypto) => Show (ShortHashed crypto a)
deriving instance Semigroup (ShortHash crypto) => Semigroup (ShortHashed crypto a)
deriving instance Monoid (ShortHash crypto) => Monoid (ShortHashed crypto a)
deriving instance ByteArrayAccess (ShortHash crypto) => ByteArrayAccess (ShortHashed crypto a)
deriving instance Serialise (ShortHash crypto) => Serialise (ShortHashed crypto a)
deriving instance ToJSON (ShortHash crypto) => ToJSON (ShortHashed crypto a)
deriving instance FromJSON (ShortHash crypto) => FromJSON (ShortHashed crypto a)

class HasHashing crypto => Hashable crypto a where
    hash      :: a -> Hashed crypto a

    shortHash :: a -> ShortHashed crypto a
    shortHash = ShortHashed . toShortHash . fromHashed . hash

class
    ( Ord (Hash crypto)
    , Eq (Hash crypto)
    ) => HasHashing crypto where
    type family HashAlgorithm crypto = algo | algo -> crypto

    -- | A hash using the default hash algorithm. Used instead of 'Hashed' when
    -- the hash pre-image is not known or cannot be typed.
    data family Hash      crypto :: *
    data family ShortHash crypto :: *

    hashByteArray :: forall ba. ByteArray.ByteArrayAccess ba => ba -> Hash crypto

    hashAlgorithm :: HashAlgorithm crypto

    -- | The zero hash. Also the minimum hash value.
    zeroHash  :: Hash crypto

    -- | The zero short hash. Also the minimum short hash value.
    zeroShortHash  :: ShortHash crypto

    -- | A short hash, in the similar spirit of the BTC one.
    -- eg. ripemd160(blake2b(x))
    toShortHash :: Hash crypto -> ShortHash crypto

    -- | Parse a 'ShortHash' from 'Text'.
    parseShortHash :: Text -> Maybe (ShortHash crypto)

    -- | A compact representation of the hash.
    compactHash :: Hash crypto -> ByteString


instance Sql.ToField (Hash crypto) => Sql.ToField (Hashed crypto a) where
    toField = Sql.toField . fromHashed

instance Fmt.Buildable (Hash crypto) => ToHttpApiData (Hash crypto) where
    toQueryParam = Fmt.sformat formatHash

-- | Tag a 'Hash' with the type it is a hash of.
toHashed :: Hash crypto -> Hashed crypto a
toHashed = Hashed

-- | Un-tag a 'Hashed' value.
fromHashed :: Hashed crypto a -> Hash crypto
fromHashed (Hashed h) = h

-- | Un-tag a 'ShortHashed' value.
fromShortHashed :: ShortHashed crypto a -> ShortHash crypto
fromShortHashed (ShortHashed h) = h

-- | Format a 'Hash' value.
--
-- This base-58 encodes the digest bytes, but does __NOT__ encode it as a
-- 'Multihash.Multihash'.
formatHash :: Fmt.Buildable (Hash crypto) => Format r (Hash crypto -> r)
formatHash = fmtB58

-- | Format a 'ShortHash' value.
formatShortHash :: Fmt.Buildable (ShortHash crypto) => Format r (ShortHash crypto -> r)
formatShortHash = "0x" Fmt.% Fmt.build

-- | Format a 'Hashed' value.
--
-- This base-58 encodes the digest bytes, but does __NOT__ encode it as a
-- 'Multihash.Multihash'.
formatHashed
    :: Fmt.Buildable (Hash crypto)
    => Format r (Hashed crypto a -> r)
formatHashed = Fmt.mapf fromHashed fmtB58

-------------------------------------------------------------------------------

-- | Hash a value's 'Binary' represenation.
hashBinary :: (HasHashing crypto, Binary a) => a -> Hashed crypto a
hashBinary = Hashed . hashByteArray . LBS.toStrict . Binary.encode

-- | Hash a values's 'Serialise' (CBOR) representation.
hashSerial :: (HasHashing crypto, Serialise a) => a -> Hashed crypto a
hashSerial = Hashed . hashByteArray . LBS.toStrict . Serial.serialise

instance HasHashing crypto => Hashable crypto () where
    hash () = toHashed zeroHash

instance HasHashing crypto => Hashable crypto ECDSA.PublicKey where
    hash = hashSerial

instance (Semigroup (Hash crypto), Hashable crypto k, Hashable crypto v) => Hashable crypto (k, v) where
    hash (k, v) = toHashed (fromHashed (hash k) <> fromHashed (hash v))

-- Internal --------------------------------------------------------------------

-- | This is just an alias over 'build', so that each crypto can implement
-- a 'Buildable' instance which makes sense.
fmtB58 :: Fmt.Buildable (Hash crypto) => Format r (Hash crypto -> r)
fmtB58 = Fmt.build
