module Oscoin.Crypto.Hash
    ( Hashed
    , Hashed'
    , Hash
    , Hash'
    , Hashable(..)
    , toHashed
    , fromHashed
    , formatHash
    , formatHashed
    , hashBinary
    , hashSerial
    , HashAlgorithm
    , hashAlgorithm
    , maxHash
    , zeroHash
    , shortHash
    ) where

import           Oscoin.Prelude
import qualified Prelude

import           Codec.Serialise.Orphans ()
import           Data.ByteArray.Orphans ()

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serial
import           Control.Monad.Fail (fail)
import           Crypto.Hash (Blake2b_256(..), Digest)
import qualified Crypto.Hash as Crypto
import           Crypto.Hash.Multi (Multihashable)
import qualified Crypto.Hash.Multi as Multihash
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Data.Aeson (FromJSON(..), ToJSON(..), withText)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Data.ByteString.BaseN (encodeBase58)
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import           Data.Maybe (fromJust)
import           Data.Tagged (Tagged(..))
import qualified Data.Text as T
import           Formatting (Format)
import qualified Formatting as Fmt
import qualified Text.Show as Show
import           Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

-- | Default hash algorithm type used in this module.
type HashAlgorithm = Blake2b_256

-- | Default hash algorithm  used in this module.
hashAlgorithm :: HashAlgorithm
hashAlgorithm = Blake2b_256

-- | Represents data that has been hashed with 'HashAlgorithm'.
type Hashed a = Hashed' HashAlgorithm a

-- | A hash using the default hash algorithm. Used instead of 'Hashed' when
-- the hash pre-image is not known or cannot be typed.
type Hash = Hash' HashAlgorithm

-- | A 'Digest' obtained by some hash algorithm @a@.
--
-- Textual representations use base-58 encoding. Serialisation and
-- deserialisation via "Crypto.Hash.Multi".
newtype Hash' a = Hash { fromHash :: Digest a }
    deriving (Eq, Ord, ByteArrayAccess)

-- FIXME: this should use multihash encoding and satisfy @read . show = id@
instance Show.Show (Hash' a) where
    show = C8.unpack . BaseN.encodedBytes . encodeBase58 . convert

instance Crypto.HashAlgorithm a => Semigroup (Hash' a) where
    (<>) a b = Hash . Crypto.hash @ByteString $ on (<>) (convert . fromHash) a b
    sconcat  = Hash . Crypto.hash @ByteString . foldMap (convert . fromHash)

instance Crypto.HashAlgorithm a => Monoid (Hash' a) where
    mempty = zeroHash

    mconcat []     = mempty
    mconcat (x:xs) = sconcat (x :| xs)

instance H.Hashable (Hash' a) where
    hashWithSalt salt (Hash d) =
        H.hashWithSalt salt (ByteArray.convert d :: ByteString)

instance Multihashable a => ToJSON (Hash' a) where
    toJSON     = toJSON . multiB58
    toEncoding = toEncoding . multiB58

instance Multihashable a => FromJSON (Hash' a) where
    parseJSON = withText "Hash'" $
        either fail pure
            . second Hash . Multihash.decodeAtBase BaseN.Base58 . encodeUtf8

instance forall a. Multihashable a => Serialise (Hash' a) where
    encode = Multihash.encodeCBOR . fromHash
    decode = Hash <$> Multihash.decodeCBOR

instance Multihashable a => ToHttpApiData (Hash' a) where
    toQueryParam = Fmt.sformat formatHash

instance Multihashable a => FromHttpApiData (Hash' a) where
    parseQueryParam =
        bimap T.pack Hash . Multihash.decodeAtBase BaseN.Base58 . encodeUtf8

-- | A 'Hash'' tagged by its pre-image
type Hashed' algo a = Tagged a (Hash' algo)

-- | Tag a 'Hash'' with the type it is a hash of.
toHashed :: Hash' algo -> Hashed' algo a
toHashed = Tagged

-- | Un-tag a 'Hashed'' value.
fromHashed :: Hashed' algo a -> Hash' algo
fromHashed = unTagged

-- | Format a 'Hash'' value.
--
-- This base-58 encodes the digest bytes, but does __NOT__ encode it as a
-- 'Multihash.Multihash'.
formatHash :: Multihashable a => Format r (Hash' a -> r)
formatHash = fmtB58

-- | Format a 'Hashed'' value.
--
-- This base-58 encodes the digest bytes, but does __NOT__ encode it as a
-- 'Multihash.Multihash'.
formatHashed :: Multihashable algo => Format r (Hashed' algo a -> r)
formatHashed = Fmt.mapf fromHashed fmtB58

-- | The maximum hash value.
maxHash :: forall a. Crypto.HashAlgorithm a => Hash' a
maxHash = Hash . fromJust $
    Crypto.digestFromByteString @a @ByteString
        (ByteArray.replicate (hashDigestSize (Proxy @a)) maxBound)

-- | The zero hash. Also the minimum hash value.
zeroHash :: forall a. Crypto.HashAlgorithm a => Hash' a
zeroHash = Hash . fromJust $
    Crypto.digestFromByteString @a @ByteString
        (ByteArray.zero (hashDigestSize (Proxy @a)))

-- | The first 7 bytes of the base-58 encoded hash.
shortHash :: Hashed a -> ByteString
shortHash = BS.take 7 . BaseN.encodedBytes . encodeBase58 . convert . fromHashed

-------------------------------------------------------------------------------

class Hashable a where
    hash :: a -> Hashed a

-- | Hash a value's 'Binary' represenation.
hashBinary :: Binary a => a -> Hashed a
hashBinary = Tagged . Hash . Crypto.hash . LBS.toStrict . Binary.encode

-- | Hash a values's 'Serialise' (CBOR) representation.
hashSerial :: Serialise a => a -> Hashed a
hashSerial = Tagged . Hash . Crypto.hash . LBS.toStrict . Serial.serialise

instance Hashable () where
    hash () = toHashed zeroHash

instance Hashable Text where
    hash = toHashed . Hash . Crypto.hash . encodeUtf8

instance Hashable ByteString where
    hash = toHashed . Hash . Crypto.hash

instance Hashable Word8 where
    hash = toHashed . Hash . Crypto.hash . BS.singleton

instance Hashable a => Hashable [a] where
    hash = toHashed . fromHashed . foldMap hash

instance Hashable a => Hashable (Seq a) where
    hash = toHashed . fromHashed . hash . toList

instance (ByteArrayAccess a) => Hashable (Maybe a) where
    hash (Just x) = toHashed . Hash $ Crypto.hash x
    hash Nothing  = toHashed $ zeroHash

instance Hashable ECDSA.PublicKey where
    hash = hashSerial

-- Internal --------------------------------------------------------------------

fmtB58 :: Multihashable a => Format r (Hash' a -> r)
fmtB58 = Fmt.mapf multiB58 BaseN.format

multiB58 :: Multihashable a => Hash' a -> BaseN.Base58
multiB58 = Multihash.encodeAtBase BaseN.Base58 . Multihash.fromDigest . fromHash

hashDigestSize :: forall proxy a. Crypto.HashAlgorithm a => proxy a -> Int
hashDigestSize _ = Crypto.hashDigestSize (Prelude.undefined :: a)
