{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Crypto.Hash
    ( Hashed
    , Hash
    , Hashable(..)
    , toHashed
    , fromHashed
    , digestFromByteString
    , hashBinary
    , hashSerial
    , HashAlgorithm
    , hashAlgorithm
    , maxHash
    , zeroHash
    , toHex
    , fromHex
    , shortHash
    ) where

import           Oscoin.Prelude

import           Crypto.Hash (Digest, Blake2b_256(..))
import qualified Crypto.Hash as Crypto
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Crypto.PubKey.ECC.ECDSA.Extended ()
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serial
import qualified Codec.Serialise.Decoding as Serial
import qualified Codec.Serialise.Encoding as Serial
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary
import           Data.Binary (Binary)
import           Data.ByteArray (convert, zero)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base16 as Base16
import           Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(String), withText)
import qualified Data.Text as T
import           Web.HttpApiData (FromHttpApiData(..))

-- | Represents data that has been hashed with 'HashAlgorithm'.
type Hashed a = Hashed' HashAlgorithm a

-- | A hash using the default hash algorithm. Used instead of 'Hashed' when
-- the hash pre-image is not known or cannot be typed.
type Hash = Digest HashAlgorithm

-- | Represents data that has been hashed with @algo@. In general, it's
-- recommended to use 'Hashed' instead.
newtype Hashed' algo a = Hashed { fromHashed :: Digest algo }
    deriving (Eq, Ord, Functor, ByteArrayAccess)

instance Hashable a => Semigroup (Hashed' HashAlgorithm a) where
    (<>) (Hashed a) (Hashed b) =
        Hashed $ Crypto.hash ((convert a :: ByteString) <> (convert b :: ByteString))

instance Hashable a => Monoid (Hashed' HashAlgorithm a) where
    mempty = Hashed zeroHash

instance Show (Hashed' algo a) where
    show = show . fromHashed

instance ToJSON (Hashed' HashAlgorithm a) where
    toJSON (Hashed digest) = toJSON digest

instance FromJSON (Hashed' HashAlgorithm a) where
    parseJSON = withText "Hashed' Blake2b_256 a" $ \t ->
        case fromHex (encodeUtf8 t) of
            Right bs -> pure $ Binary.decode (LBS.fromStrict bs)
            Left err -> fail (T.unpack $ fromError err)

instance Binary (Hashed' HashAlgorithm a) where
    put = Binary.put <$> fromHashed
    get = Hashed <$> Binary.get

instance Serialise (Hashed' HashAlgorithm a) where
    encode = Serial.encode <$> fromHashed
    decode = Hashed <$> Serial.decode

instance FromHttpApiData (Hashed' HashAlgorithm a) where
    parseQueryParam txt =
        case fromHex (encodeUtf8 txt) of
            Left err -> Left (fromError err)
            Right bs -> Right $ Binary.decode $ LBS.fromStrict bs

-- | Wrap a 'Crypto.Digest' 'HashAlgorithm' into a 'Hashed'.
toHashed :: Crypto.Digest HashAlgorithm -> Hashed a
toHashed = Hashed

-- | Convert a 'ByteString' to a 'Digest'. Throws an error if the input is
-- not a valid digest.
digestFromByteString :: HasCallStack => ByteString -> Crypto.Digest HashAlgorithm
digestFromByteString bs =
    fromMaybe (error "Oscoin.Crypto.Hash: Invalid input")
              (Crypto.digestFromByteString bs)

-- | Default hash algorithm type used in this module.
type HashAlgorithm = Blake2b_256

-- | Default hash algorithm  used in this module.
hashAlgorithm :: Blake2b_256
hashAlgorithm = Blake2b_256

instance Binary (Digest HashAlgorithm) where
    put digest =
        Binary.putByteString (convert digest :: ByteString)
    get =
        fromJust . Crypto.digestFromByteString <$> Binary.getByteString size
      where
        size = Crypto.hashDigestSize Blake2b_256

instance Serialise (Digest HashAlgorithm) where
    encode digest =
        Serial.encodeBytes (convert digest :: ByteString)
    decode =
        fromJust . Crypto.digestFromByteString <$> Serial.decodeBytes

instance ToJSON (Digest HashAlgorithm) where
    toJSON =
        String . decodeUtf8 . toHex . LBS.toStrict . Binary.encode

-- | The maximum hash value.
maxHash :: forall a. Crypto.HashAlgorithm a => Digest a
maxHash = fromJust $
    Crypto.digestFromByteString (ByteArray.replicate n maxBound :: ByteString)
  where
    n = Crypto.hashDigestSize (undefined :: a)

-- | The zero hash. Also the minimum hash value.
zeroHash :: forall a. Crypto.HashAlgorithm a => Digest a
zeroHash = fromJust $
    Crypto.digestFromByteString (zero n :: ByteString)
  where
    n = Crypto.hashDigestSize (undefined :: a)

toHex :: ByteArrayAccess ba => ba -> ByteString
toHex =
    Base16.encode . convert

-- TODO: Make result type polymorphic: `Either Error ba`.
fromHex :: ByteString -> Either Error ByteString
fromHex bs =
    case Base16.decode bs of
        (valid, "")  -> Right valid
        (_, invalid) -> Left $ Error ("Can't parse " <> tshow invalid)

shortHash :: Hashed a -> ByteString
shortHash =
    BS.take 7 . toHex

-------------------------------------------------------------------------------

class Hashable a where
    hash :: a -> Hashed a

hashBinary :: Binary a => a -> Hashed a
hashBinary = Hashed . Crypto.hash . LBS.toStrict . Binary.encode

hashSerial :: Serialise a => a -> Hashed a
hashSerial = Hashed . Crypto.hash . LBS.toStrict . Serial.serialise

instance Hashable () where
    hash () = Hashed zeroHash

instance Hashable Text where
    hash = Hashed . Crypto.hash . encodeUtf8

instance Hashable ByteString where
    hash = Hashed . Crypto.hash

instance Hashable Word8 where
    hash = Hashed . Crypto.hash . BS.singleton

instance Hashable a => Hashable [a] where
    hash = toHashed . fromHashed . foldMap hash

instance Hashable a => Hashable (Seq a) where
    hash = toHashed . fromHashed . hash . toList

instance (ByteArrayAccess a) => Hashable (Maybe a) where
    hash (Just x) = Hashed (Crypto.hash x)
    hash Nothing  = Hashed zeroHash

instance Hashable ECDSA.PublicKey where
    hash = hashBinary
