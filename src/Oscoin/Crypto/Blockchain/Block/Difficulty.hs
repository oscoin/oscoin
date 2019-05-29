module Oscoin.Crypto.Blockchain.Block.Difficulty
    ( Difficulty
    , TargetDifficulty
    , fromDifficulty
    , unsafeDifficulty
    , minDifficulty
    , maxDifficulty
    , easyDifficulty
    , parseDifficulty
    , prettyDifficulty
    , encodeDifficulty
    , decodeDifficulty
    , readWord32LE
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise(..))
import qualified Codec.Serialise.Decoding as Serialise
import qualified Codec.Serialise.Encoding as Serialise
import           Control.Monad (fail)
import qualified Data.Binary.Builder as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import           Database.SQLite.Simple.FromField (FromField(..))
import           Database.SQLite.Simple.ToField (ToField(..))

-- | Block difficulty.
--
-- Encoded in the same compact 4-byte format as the `bits` field in the
-- Bitcoin block header.
--
-- See <https://en.bitcoin.it/wiki/Difficulty> for more details.
--
-- The encoding to compact form is handled by 'encodeDifficulty' and
-- 'decodeDifficulty'.
--
newtype Difficulty = Difficulty { fromDifficulty :: Word32 }
    deriving (Show, Read, Eq, Ord, Enum, FromField, ToField)

instance Serialise Difficulty where
    encode = Serialise.encodeBytes
           . LBS.toStrict
           . BS.toLazyByteString
           . Binary.putWord32le
           . fromDifficulty
    decode = do
        bs <- LBS.fromStrict <$> Serialise.decodeBytes
        difi <- either (\(_,_,e) -> fail e) (\(_,_,x) -> pure (Difficulty x)) $
            Binary.runGetOrFail Binary.getWord32le bs
        case decodeDifficulty difi of
            (_, True)  -> fail "Overflow trying to decode difficulty"
            (_, False) -> pure difi

unsafeDifficulty :: Word32 -> Difficulty
unsafeDifficulty = Difficulty

prettyDifficulty :: Difficulty -> Text
prettyDifficulty =
     BaseN.encodedText . BaseN.encodeBase16 . toBS . fromDifficulty
  where
    toBS = LBS.toStrict . BS.toLazyByteString . BS.word32LE

parseDifficulty :: Text -> Maybe Difficulty
parseDifficulty t = do
    n <- BaseN.decodeBase16 $ encodeUtf8 t
    d <- Difficulty <$> readWord32LE n
    case decodeDifficulty d of
        (_, True)  -> Nothing -- Overflow!
        (_, False) -> Just d

-- | The minimum difficulty.
minDifficulty :: Difficulty
minDifficulty = encodeDifficulty
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | An easy difficulty. About 24s per block on a single core.
easyDifficulty :: Difficulty
easyDifficulty = encodeDifficulty
    0x00000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

maxDifficulty :: Difficulty
maxDifficulty =
    Difficulty 0x1d00ffff

-- | Decode the compact number used in the difficulty target of a block.
--
-- Taken from 'haskoin-core' project.
--
-- The compact format is a representation of a whole number \(N\) using an
-- unsigned 32-bit number similar to a floating point format. The most
-- significant 8 bits are the unsigned exponent of base 256. This exponent can
-- be thought of as the number of bytes of \(N\). The lower 23 bits are the
-- mantissa. Bit number 24 represents the sign of \(N\).
--
-- \[
-- N = -1^{sign} \times mantissa \times 256^{exponent-3}
-- \]
decodeDifficulty :: Difficulty -> (Integer, Bool) -- ^ 'True' means overflow
decodeDifficulty (Difficulty nCompact) =
    (if neg then res * (-1) else res, over)
  where
    nSize = fromIntegral nCompact `shiftR` 24

    nWord' = nCompact .&. 0x007fffff

    nWord | nSize <= 3 = nWord' `shiftR` (8 * (3 - nSize))
          | otherwise  = nWord'

    res :: Integer
    res | nSize <= 3 = fromIntegral nWord
        | otherwise  = fromIntegral nWord `shiftL` (8 * (nSize - 3))

    neg = nWord /= 0 && (nCompact .&. 0x00800000) /= 0

    over = nWord /= 0 && (nSize > 34 ||
                          nWord > 0xff && nSize > 33 ||
                          nWord > 0xffff && nSize > 32)

-- | Encode an 'Integer' to the compact number format used in the difficulty
-- target of a block.
--
-- Taken from 'haskoin-core' project.
--
encodeDifficulty :: Integer -> Difficulty
encodeDifficulty i =
    Difficulty nCompact
  where
    i' = abs i

    neg = i < 0

    nSize' = let f 0 = 0
                 f n = 1 + f (n `shiftR` 8)
             in f i'

    nCompact'''
        | nSize' <= 3 = fromIntegral $ (low64 .&. i') `shiftL` (8 * (3 - nSize'))
        | otherwise   = fromIntegral $ low64 .&. (i' `shiftR` (8 * (nSize' - 3)))

    (nCompact'', nSize)
        | nCompact''' .&. 0x00800000 /= 0 = (nCompact''' `shiftR` 8, nSize' + 1)
        | otherwise                       = (nCompact''', nSize')

    nCompact' = nCompact'' .|. (fromIntegral nSize `shiftL` 24)

    nCompact :: Word32
    nCompact | neg && (nCompact' .&. 0x007fffff /= 0) = nCompact' .|. 0x00800000
             | otherwise = nCompact'

    low64 :: Integer
    low64 = 0xffffffffffffffff

-- | Convert a 4-byte 'ByteString' into a 'Word32'.
--
-- Adapted from the cardano project.
--
readWord32LE :: ByteString -> Maybe Word32
readWord32LE bs = if BS.length bs /= 4 then Nothing else Just $
    fromIntegral (BS.unsafeIndex bs 0)
  + fromIntegral (BS.unsafeIndex bs 1) `shiftL` 8
  + fromIntegral (BS.unsafeIndex bs 2) `shiftL` 16
  + fromIntegral (BS.unsafeIndex bs 3) `shiftL` 24

-- | Minimum difficulty a 'Block' must have to be considered valid.
--
-- Invariant: 'Difficulty' must always be lower (harder) than 'TargetDifficulty' for
-- valid blocks.
type TargetDifficulty = Difficulty
