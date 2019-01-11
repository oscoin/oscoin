module Oscoin.Crypto.Blockchain.Block.Difficulty
    ( Difficulty(..)
    , TargetDifficulty
    , minDifficulty
    , easyDifficulty
    , parseDifficulty
    , prettyDifficulty
    , encodeDifficultyCompact
    , decodeDifficultyCompact
    , readWord32LE
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise(..))
import           Control.Exception (assert)
import           Control.Monad (fail)
import           Crypto.Number.Serialize (i2osp, os2ip)
import           Data.Aeson (FromJSON(..), ToJSON(..), withText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
                 (FromField(..), fieldData, returnError)
import           Database.SQLite.Simple.Ok (Ok(..))
import           Database.SQLite.Simple.ToField (ToField(..))

-- | Block difficulty.
--
-- Uses an 'Integer' underneath for ease of use, but is encoded into a compact
-- 4-byte format for storage and transmission. The compact format is the
-- same that is used in Bitcoin.
--
-- See <https://en.bitcoin.it/wiki/Difficulty> for more details.
--
-- The encoding to compact form is handled by 'encodeDifficultyCompact' and
-- 'decodeDifficultyCompact'.
--
newtype Difficulty = Difficulty { fromDifficulty :: Integer }
    deriving (Show, Read, Eq, Ord, Num, Enum, Real, Integral)

instance Serialise Difficulty where
    encode = encode . encodeDifficultyCompact
    decode = do
        compact <- decode
        case decodeDifficultyCompact compact of
            (d, False) -> pure d
            (_, True)  -> fail "Difficulty overflowed"

instance FromField Difficulty where
    fromField f =
        case fieldData f of
            SQLBlob bs -> case decodeDifficultyCompact (readWord32LE bs) of
                (d, False) ->
                    Ok d
                (_, True)  ->
                    returnError ConversionFailed f "Difficulty overflowed"
            _ ->
                returnError ConversionFailed f "Invalid SQL type for Difficulty"

instance ToField Difficulty where
    toField = SQLBlob . LBS.toStrict . BS.toLazyByteString . BS.word32LE
                      . encodeDifficultyCompact

instance ToJSON Difficulty where
    toJSON = toJSON . prettyDifficulty

instance FromJSON Difficulty where
    parseJSON = withText "Difficulty" $ \t ->
        case parseDifficulty t of
            Just d  -> pure d
            Nothing -> fail "Error decoding difficulty"

prettyDifficulty :: Difficulty -> Text
prettyDifficulty =
     BaseN.encodedText . BaseN.encodeBase16 . i2osp . fromDifficulty

parseDifficulty :: Text -> Maybe Difficulty
parseDifficulty t =
    case BaseN.decodeBase16 $ encodeUtf8 t of
        Just d  -> Just $ Difficulty (os2ip d)
        Nothing -> Nothing

-- | The minimum difficulty.
minDifficulty :: Difficulty
minDifficulty =
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | An easy difficulty. About 24s per block on a single core.
easyDifficulty :: Difficulty
easyDifficulty =
    0x00000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

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
decodeDifficultyCompact :: Word32 -> (Difficulty, Bool) -- ^ 'True' means overflow
decodeDifficultyCompact nCompact =
    (Difficulty $ if neg then res * (-1) else res, over)
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
encodeDifficultyCompact :: Difficulty -> Word32
encodeDifficultyCompact (Difficulty i) =
    nCompact
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
readWord32LE :: ByteString -> Word32
readWord32LE bs = assert (BS.length bs == 4) $
    fromIntegral (BS.unsafeIndex bs 0)
  + fromIntegral (BS.unsafeIndex bs 1) `shiftL` 8
  + fromIntegral (BS.unsafeIndex bs 2) `shiftL` 16
  + fromIntegral (BS.unsafeIndex bs 3) `shiftL` 24

-- | Minimum difficulty a 'Block' must have to be considered valid.
--
-- Invariant: 'Difficulty' must always be lower (harder) than 'TargetDifficulty' for
-- valid blocks.
type TargetDifficulty = Difficulty
