{-# LANGUAGE DataKinds #-}

-- | Kitchen sink base-N encoding and decoding of strict 'ByteStrings'.
module Data.ByteString.BaseN
    ( Base(..)

    , AtBase
    , encodedBytes
    , encodedBuilder

    , Base2
    , Base16
    , Base58
    , Base64

    -- * Compact Representation
    , AtBaseCompact
    , compact
    , expand

    -- * Tagged
    -- $tagged
    , Base16Of
    , Base58Of
    , Base64Of

    -- ** Re-exports
    , tagWith
    , unTagged

    -- ** CBOR
    -- $cbor
    , DeserialiseError(..)
    , deserialiseAtBase

    -- * Encoding
    -- $encoding
    , encodeBase16
    , encodeBase58
    , encodeBase64
    , encodeAtBase

    -- * Decoding Bytes
    -- $decodingbytes
    , DecodeBase
    , decodeBase16
    , decodeBase16Either
    , decodeBase58
    , decodeBase58Either
    , decodeBase64
    , decodeBase64Either
    , decodeBase64Lenient
    , decodeAtBase
    , decodeAtBaseEither

    -- * Decoding
    , decode

    -- ** Untrusted Input
    -- $untrusted
    , ValidBase
    , validBase16
    , validBase16Either
    , validBase58
    , validBase58Either
    , validBase64
    , validBase64Either
    , validAtBase
    , validAtBaseEither

    , validAndDecoded
    , validAndDecodedEither

    -- * 'Text'
    , encodedTextAtBase
    , encodedText
    , encodedTextBuilder

    -- * 'Formatting'
    , format
    , formatAtBase
    ) where

import           Prelude

import           Codec.Serialise
                 (DeserialiseFailure, Serialise, deserialiseOrFail)
import           Control.DeepSeq (NFData)
import           Data.Aeson
                 (FromJSON(..), FromJSONKey, ToJSON(..), ToJSONKey, withText)
import qualified Data.Aeson.Encoding as JSON
import           Data.Bifunctor (bimap, first, second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Base64 as Base64
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Lazy (fromStrict)
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.Hashable (Hashable)
import           Data.Proxy (Proxy(..))
import           Data.String (IsString(..))
import           Data.Tagged (Tagged, tagWith, unTagged)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.Text.Lazy.Builder as T
import qualified Formatting
import           GHC.TypeLits (KnownNat, Nat, natVal)
import           Text.Show (Show(..), showParen, showString)

-- | Supported bases.
data Base (a :: Nat) where
    Base2  :: Base  2
    Base16 :: Base 16
    Base58 :: Base 58
    Base64 :: Base 64

-- | A 'ByteString' encoded at a specific base.
newtype AtBase (b :: Nat) = BaseN { fromAtBase :: ByteString }
    deriving (Eq, Ord, Hashable, NFData)

-- | Extract the base-n encoded bytes from an 'AtBase'.
--
-- To recover the original 'ByteString' (*not* base-n encoded), use 'decode'.
encodedBytes :: AtBase b -> ByteString
encodedBytes (BaseN bs) = bs

-- | Like 'encodedBytes', but return a 'Builder'.
encodedBuilder :: AtBase b -> Builder
encodedBuilder = Builder.byteString . encodedBytes

instance KnownNat b => Show (AtBase b) where
    showsPrec p (BaseN bs) = showParen (p >= 11)
        ( showString ("Base" <> show (natVal (Proxy @b)) <> " ")
        . showsPrec 11 bs
        )

instance ValidBase b => IsString (AtBase b) where
    fromString = either error id . validAtBaseEither (Proxy @b) . C8.pack

instance ToJSON (AtBase b) where
    toJSON     = toJSON . encodedText
    toEncoding = JSON.text . encodedText

instance (ValidBase b, KnownNat b) => FromJSON (AtBase b) where
    parseJSON =
        withText ("AtBase " <> show (natVal (Proxy @b))) $
            either fail pure . validAtBaseEither (Proxy @b) . encodeUtf8

instance ToJSONKey (AtBase b)
instance (ValidBase b, KnownNat b) => FromJSONKey (AtBase b)

type Base2  = AtBase  2
type Base16 = AtBase 16
type Base58 = AtBase 58
type Base64 = AtBase 64

-- Compact ---------------------------------------------------------------------

-- | A more memory-efficient representation of base-n encoded bytes.
--
-- Uses 'ShortByteString', recommendations and caveats described there apply.
newtype AtBaseCompact (b :: Nat) = BaseNShort
    { fromAtBaseCompact :: ShortByteString
    } deriving (Eq, Ord, Hashable, NFData)

instance KnownNat b => Show (AtBaseCompact b) where
    showsPrec p (BaseNShort bs) = showParen (p >= 11)
        ( showString ("Base" <> show (natVal (Proxy @b)) <> "Compact ")
        . showsPrec 11 bs
        )

compact :: AtBase b -> AtBaseCompact b
compact = BaseNShort . Short.toShort . fromAtBase

expand :: AtBaseCompact b -> AtBase b
expand = BaseN . Short.fromShort . fromAtBaseCompact

-- $tagged
-- 'AtBase' values tagged by the type they're representing.

type Base16Of a = Tagged a (AtBase 16)
type Base58Of a = Tagged a (AtBase 58)
type Base64Of a = Tagged a (AtBase 64)

-- $cbor
-- Directly go from (presumed to be) base-n encoded 'ByteString' to
-- de-'Serialise'-able value.

data DeserialiseError =
      DecodeBaseError  String
    | DeserialiseError DeserialiseFailure
    deriving Show

deserialiseAtBase
    :: ( Serialise  a
       , DecodeBase b
       )
    => proxy b
    -> ByteString
    -> Either DeserialiseError a
deserialiseAtBase base bs = do
    bs' <- bimap DecodeBaseError fromStrict $ decodeAtBaseEither base bs
    first DeserialiseError $ deserialiseOrFail bs'

-- $encoding

encodeBase16 :: ByteString -> AtBase 16
encodeBase16 = BaseN . Base16.encode
{-# INLINE encodeBase16 #-}

encodeBase58 :: ByteString -> AtBase 58
encodeBase58 = BaseN . Base58.encodeBase58 Base58.bitcoinAlphabet
{-# INLINE encodeBase58 #-}

encodeBase64 :: ByteString -> AtBase 64
encodeBase64 = BaseN . Base64.encode
{-# INLINE encodeBase64 #-}

-- | Encode at a base supplied at runtime.
encodeAtBase :: Base b -> ByteString -> AtBase b
encodeAtBase Base2  = BaseN
encodeAtBase Base16 = encodeBase16
encodeAtBase Base58 = encodeBase58
encodeAtBase Base64 = encodeBase64

-- $decodingbytes
-- Decode (presumed to be) base-n encoded 'ByteString's to their original
-- (base-2) value.

decodeBase16 :: ByteString -> Maybe ByteString
decodeBase16 = either (const Nothing) pure . decodeBase16Either

decodeBase16Either :: ByteString -> Either String ByteString
decodeBase16Either bs =
    case Base16.decode bs of
        (x, "")      -> Right x
        (x, invalid) -> Left . mconcat $
            [ "Decoded: "
            , "`", unpack x, "`"
            , " until invalid sequence: "
            , "`", unpack invalid, "`"
            ]
{-# INLINE decodeBase16Either #-}

decodeBase58 :: ByteString -> Maybe ByteString
decodeBase58 = Base58.decodeBase58 Base58.bitcoinAlphabet
{-# INLINE decodeBase58 #-}

decodeBase58Either :: ByteString -> Either String ByteString
decodeBase58Either =
    maybe (Left "Invalid characters in base58 string") Right . decodeBase58

decodeBase64 :: ByteString -> Maybe ByteString
decodeBase64 = either (const Nothing) pure . decodeBase64Either

decodeBase64Either :: ByteString -> Either String ByteString
decodeBase64Either = Base64.decode
{-# INLINE decodeBase64Either #-}

decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = Base64.decodeLenient
{-# INLINE decodeBase64Lenient #-}

class DecodeBase (b :: Nat) where
    decodeAtBase       :: proxy b -> ByteString -> Maybe ByteString
    decodeAtBaseEither :: proxy b -> ByteString -> Either String ByteString

instance DecodeBase 2 where
    decodeAtBase       = const pure
    decodeAtBaseEither = const pure
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase 16 where
    decodeAtBase       = const decodeBase16
    decodeAtBaseEither = const decodeBase16Either
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase 58 where
    decodeAtBase       = const decodeBase58
    decodeAtBaseEither = const decodeBase58Either
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase 64 where
    decodeAtBase       = const decodeBase64
    decodeAtBaseEither = const decodeBase64Either
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

-- | Recover the original 'ByteString' of a base-n encoded value.
decode :: DecodeBase b => AtBase b -> ByteString
decode at = case decodeAtBaseEither at (encodedBytes at) of
    Left  e -> error $ "Impossible: invalid base encoding: " <> e
    Right b -> b

-- $untrusted
-- Construct 'AtBase's from raw 'ByteString's. Note that this attempts to decode
-- using the functions from $decoding, and throws away the result.

validBase16 :: ByteString -> Maybe (AtBase 16)
validBase16 bs = const (BaseN bs) <$> decodeBase16 bs

validBase16Either :: ByteString -> Either String (AtBase 16)
validBase16Either bs = second (const $ BaseN bs) $ decodeBase16Either bs

validBase58 :: ByteString -> Maybe (AtBase 58)
validBase58 bs = const (BaseN bs) <$> decodeBase58 bs

validBase58Either :: ByteString -> Either String (AtBase 58)
validBase58Either bs = second (const $ BaseN bs) $ decodeBase58Either bs

validBase64 :: ByteString -> Maybe (AtBase 64)
validBase64 bs = const (BaseN bs) <$> decodeBase64 bs

validBase64Either :: ByteString -> Either String (AtBase 64)
validBase64Either bs = second (const $ BaseN bs) $ decodeBase64Either bs

class ValidBase (b :: Nat) where
    validAtBase       :: proxy b -> ByteString -> Maybe (AtBase b)
    validAtBaseEither :: proxy b -> ByteString -> Either String (AtBase b)

instance ValidBase 2 where
    validAtBase       = const (pure . BaseN)
    validAtBaseEither = const (pure . BaseN)
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase 16 where
    validAtBase       = const validBase16
    validAtBaseEither = const validBase16Either
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase 58 where
    validAtBase       = const validBase58
    validAtBaseEither = const validBase58Either
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase 64 where
    validAtBase       = const validBase64
    validAtBaseEither = const validBase64Either
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

-- | Like 'validAtBase', but also return the decoded 'ByteString'.
validAndDecoded
    :: DecodeBase b
    => proxy b
    -> ByteString
    -> Maybe (AtBase b, ByteString)
validAndDecoded at bs = (BaseN bs,) <$> decodeAtBase at bs

-- | Like 'validAtBaseEither', but also return the decoded 'ByteString'.
validAndDecodedEither
    :: DecodeBase b
    => proxy b
    -> ByteString
    -> Either String (AtBase b, ByteString)
validAndDecodedEither at bs = (BaseN bs,) <$> decodeAtBaseEither at bs

-- Text ------------------------------------------------------------------------

-- | Like 'encodeAtBase', but from a 'Text' value.
encodedTextAtBase :: Base b -> Text -> AtBase b
encodedTextAtBase b = encodeAtBase b . encodeUtf8
{-# INLINE encodedTextAtBase #-}

-- | Like 'encodedBytes', but returns a 'Text' value.
encodedText :: AtBase b -> Text
encodedText (BaseN bs) = decodeLatin1 bs
{-# INLINE encodedText #-}

-- | Like 'encodedBuilder', but returns a text 'T.Builder'.
encodedTextBuilder :: AtBase b -> T.Builder
encodedTextBuilder = T.fromText . encodedText
{-# INLINE encodedTextBuilder #-}

-- Formatting ------------------------------------------------------------------

-- | Format a base-n encoded value.
format, formatAtBase :: Formatting.Format r (AtBase b -> r)
format = Formatting.later encodedTextBuilder

formatAtBase = format
{-# INLINE formatAtBase #-}
