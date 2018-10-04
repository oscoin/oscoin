-- | \"multihash\" encoding of 'Crypto.Hash.Digest's.
--
-- See <https://github.com/multiformats/multihash> for the \"specification\" of
-- multihash encoding.
module Crypto.Hash.Multi
    ( Multihash
    , Multihashable
    , DecodeError

    , fromDigest
    , encodedBytes
    , multihash
    , encodeAtBase
    , decodeAtBase
    , decodeBytes
    , encodeCBOR
    , decodeCBOR

    , HashAlgorithm(..)
    ) where

import           Prelude

import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Applicative (liftA2)
import           Control.Monad ((>=>))
import           Control.Monad.Fail (MonadFail)
import qualified Crypto.Hash as C
import           Data.Bifunctor (bimap)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import           Data.Bits
                 (Bits, clearBit, setBit, shiftL, shiftR, testBit, (.|.))
import           Data.ByteArray (ByteArrayAccess, convert)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Proxy (Proxy(..))
import           Data.Word (Word16, Word8)

-- | A multihash-encoded strict 'ByteString'.
newtype Multihash = Multihash ByteString

type Multihashable a = (C.HashAlgorithm a, FromCryptonite a)

data DecodeError =
      UnknownHashAlgorithm
    | AlgorithmMismatch
    | InvalidDigest

-- | Encode a 'C.Digest' as a 'Multihash'.
fromDigest :: forall a. Multihashable a => C.Digest a -> Multihash
fromDigest dig =
    Multihash
        . toStrict . B.toLazyByteString
        $ code <> len <> B.byteString bytes
  where
    code  = buildVarInt . toCode $ fromCryptonite dig
    bytes = convert dig
    len   = buildVarInt $ BS.length bytes

-- | Extract the raw, multihash-encoded bytes of a 'Multihash'.
encodedBytes :: Multihash -> ByteString
encodedBytes (Multihash bs) = bs

-- | Hash a value to a 'Multihash'
multihash :: (ByteArrayAccess ba, Multihashable a) => a -> ba -> Multihash
multihash rithm = fromDigest . C.hashWith rithm

-- | Encode a 'Multihash' at a 'BaseN.Base'.
encodeAtBase :: BaseN.Base b -> Multihash -> BaseN.AtBase b
encodeAtBase base (Multihash bs) = BaseN.encodeAtBase base bs

-- | Decode a 'C.Digest' from a multihash- and base-n-encoded 'ByteString'.
decodeAtBase
    :: forall a b.
       ( Multihashable    a
       , BaseN.DecodeBase b
       )
    => BaseN.Base b
    -> ByteString
    -> Either String (C.Digest a)
decodeAtBase base = BaseN.decodeAtBaseEither base >=> decodeBytes

-- | Decode a 'C.Digest' from a multihash-encoded 'ByteString'.
decodeBytes
    :: forall a. Multihashable a
    => ByteString
    -> Either String (C.Digest a)
decodeBytes = bimap _3 _3 . Binary.runGetOrFail getMultihash . fromStrict
  where
    _3 (_,_,x) = x

-- | Multihash-like CBOR 'CBOR.Encoding' of a 'C.Digest'.
encodeCBOR :: Multihashable a => C.Digest a -> CBOR.Encoding
encodeCBOR dig =
       CBOR.encodeListLen 3
    <> CBOR.encodeWord 0    -- constructor tag / version
    <> CBOR.encode (toCode (fromCryptonite dig))
    <> CBOR.encodeBytes (convert dig)

-- | CBOR 'CBOR.Decoder' to decode a 'C.Digest' encoded via 'encodeCBOR'.
decodeCBOR :: forall s a. Multihashable a => CBOR.Decoder s (C.Digest a)
decodeCBOR = do
    pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
    case pre of
        (3, 0) -> do
            rithm <- fromCode <$> CBOR.decode
            case rithm of
                Nothing -> failWith UnknownHashAlgorithm
                Just  a
                    | fromCryptonite (Proxy @a) /= a ->
                        failWith AlgorithmMismatch
                    | otherwise -> do
                        bytes <- CBOR.decodeBytes
                        maybe (failWith InvalidDigest) pure
                            $ C.digestFromByteString bytes

        _ -> fail "Multihash: Invalid Tag"

-- Internal --------------------------------------------------------------------

getMultihash :: forall a. Multihashable a => Binary.Get (C.Digest a)
getMultihash = do
    code <- Binary.getWord8 >>= getVarInt
    case fromCode code of
        Nothing -> failWith UnknownHashAlgorithm
        Just a' | fromCryptonite (Proxy @a) /= a' -> failWith AlgorithmMismatch
                | otherwise -> do
            len <- Binary.getWord8 >>= getVarInt
            dig <- Binary.getByteString len
            maybe (failWith InvalidDigest) pure $ C.digestFromByteString dig

buildVarInt :: (Integral a, Bits a) => a -> B.Builder
buildVarInt n
    | n < 0x80  = B.word8 $ fromIntegral n
    | otherwise = B.word8 (setBit (fromIntegral n) 7) <> buildVarInt (shiftR n 7)

getVarInt :: (Num a, Bits a) => Word8 -> Binary.Get a
getVarInt n
    | testBit n 7 = do
        v <- Binary.getWord8 >>= getVarInt
        pure $ shiftL v 7 .|. clearBit (fromIntegral n) 7
    | otherwise   = pure $ fromIntegral n

-- | 'Crypto.Hash.HashAlgorithm's for which we know a multihash code.
--
-- Note that this currently excludes variable output-length algorithms.
data HashAlgorithm =
      Blake2s_160
    | Blake2s_224
    | Blake2s_256
    | Blake2b_160
    | Blake2b_224
    | Blake2b_256
    | Blake2b_384
    | Blake2b_512
    | MD4
    | MD5
    | SHA1
    | SHA256
    | SHA512
    | Keccak_224
    | Keccak_256
    | Keccak_384
    | Keccak_512
    | SHA3_224
    | SHA3_256
    | SHA3_384
    | SHA3_512
    deriving (Eq, Enum, Bounded)

class FromCryptonite a where
    fromCryptonite :: proxy a -> HashAlgorithm

instance FromCryptonite C.Blake2s_160 where fromCryptonite _ = Blake2s_160
instance FromCryptonite C.Blake2s_224 where fromCryptonite _ = Blake2s_224
instance FromCryptonite C.Blake2s_256 where fromCryptonite _ = Blake2s_256
instance FromCryptonite C.Blake2b_160 where fromCryptonite _ = Blake2b_160
instance FromCryptonite C.Blake2b_224 where fromCryptonite _ = Blake2b_224
instance FromCryptonite C.Blake2b_256 where fromCryptonite _ = Blake2b_256
instance FromCryptonite C.Blake2b_384 where fromCryptonite _ = Blake2b_384
instance FromCryptonite C.Blake2b_512 where fromCryptonite _ = Blake2b_512
instance FromCryptonite C.MD4         where fromCryptonite _ = MD4
instance FromCryptonite C.MD5         where fromCryptonite _ = MD5
instance FromCryptonite C.SHA1        where fromCryptonite _ = SHA1
instance FromCryptonite C.SHA256      where fromCryptonite _ = SHA256
instance FromCryptonite C.SHA512      where fromCryptonite _ = SHA512
instance FromCryptonite C.Keccak_224  where fromCryptonite _ = Keccak_224
instance FromCryptonite C.Keccak_256  where fromCryptonite _ = Keccak_256
instance FromCryptonite C.Keccak_384  where fromCryptonite _ = Keccak_384
instance FromCryptonite C.Keccak_512  where fromCryptonite _ = Keccak_512
instance FromCryptonite C.SHA3_224    where fromCryptonite _ = SHA3_224
instance FromCryptonite C.SHA3_256    where fromCryptonite _ = SHA3_256
instance FromCryptonite C.SHA3_384    where fromCryptonite _ = SHA3_384
instance FromCryptonite C.SHA3_512    where fromCryptonite _ = SHA3_512

toCode :: HashAlgorithm -> Word16
toCode Blake2s_160 = 0xb254
toCode Blake2s_224 = 0xb25c
toCode Blake2s_256 = 0xb260
toCode Blake2b_160 = 0xb214
toCode Blake2b_224 = 0xb21c
toCode Blake2b_256 = 0xb220
toCode Blake2b_384 = 0xb230
toCode Blake2b_512 = 0xb240
toCode MD4         = 0xd4
toCode MD5         = 0xd5
toCode SHA1        = 0x11
toCode SHA256      = 0x12
toCode SHA512      = 0x13
toCode Keccak_224  = 0x1A
toCode Keccak_256  = 0x1B
toCode Keccak_384  = 0x1C
toCode Keccak_512  = 0x1D
toCode SHA3_224    = 0x17
toCode SHA3_256    = 0x16
toCode SHA3_384    = 0x15
toCode SHA3_512    = 0x14

fromCode :: Word16 -> Maybe HashAlgorithm
fromCode 0xb254 = pure Blake2s_160
fromCode 0xb25c = pure Blake2s_224
fromCode 0xb260 = pure Blake2s_256
fromCode 0xb214 = pure Blake2b_160
fromCode 0xb21c = pure Blake2b_224
fromCode 0xb220 = pure Blake2b_256
fromCode 0xb230 = pure Blake2b_384
fromCode 0xb240 = pure Blake2b_512
fromCode 0xd4   = pure MD4
fromCode 0xd5   = pure MD5
fromCode 0x11   = pure SHA1
fromCode 0x12   = pure SHA256
fromCode 0x13   = pure SHA512
fromCode 0x1A   = pure Keccak_224
fromCode 0x1B   = pure Keccak_256
fromCode 0x1C   = pure Keccak_384
fromCode 0x1D   = pure Keccak_512
fromCode 0x17   = pure SHA3_224
fromCode 0x16   = pure SHA3_256
fromCode 0x15   = pure SHA3_384
fromCode 0x14   = pure SHA3_512
fromCode _      = Nothing

--------------------------------------------------------------------------------

failWith :: MonadFail m => DecodeError -> m a
failWith UnknownHashAlgorithm = fail "UnknownHashAlgorithm"
failWith AlgorithmMismatch    = fail "AlgorithmMismatch"
failWith InvalidDigest        = fail "InvalidDigest"
