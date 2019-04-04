-- | Partial, opinionated support for Bech32 (BIP 173).
-- Mostly taken and adapted from the reference implementation available
-- <here https://github.com/sipa/bech32/blob/master/ref/haskell/src/Codec/Binary/Bech32.hs>
module Oscoin.Crypto.Address.Bech32
    ( checksum
    , verifyChecksum

    -- * Internals
    , bech32checksum
    , verifyBech32checksum
    ) where

import           Oscoin.Prelude

import           Data.Bits
import qualified Data.ByteString as BS
import           Oscoin.Crypto.Address.Internal (Checksum(..))

(.>>.), (.<<.) :: Bits a => a -> Int -> a
(.>>.) = unsafeShiftR
(.<<.) = unsafeShiftL

asWord5 :: Word8 -> Word8
asWord5 w8 = w8 .&. 31

bech32Polymod :: [Word8] -> Word
bech32Polymod (map asWord5 -> values) = foldl' go 1 values .&. 0x3fffffff
  where
    go chk value = foldl' xor chk' [g | (g, i) <- zip generator [25..], testBit chk i]
      where
        generator = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]
        chk' = chk .<<. 5 `xor` fromIntegral value

bech32HRPExpand :: ByteString -> [Word8]
bech32HRPExpand hrp =
    let unpacked = BS.unpack hrp
    in map (.>>. 5) unpacked <> [0] <> map asWord5 unpacked

bech32checksum :: ByteString -> [Word8] -> [Word8]
bech32checksum hrp dat = [asWord5 . fromIntegral $ (polymod .>>. i) | i <- [25,20..0]]
  where
    values = bech32HRPExpand hrp <> dat
    polymod = bech32Polymod (values <> [0, 0, 0, 0, 0, 0]) `xor` 1

verifyBech32checksum :: ByteString -> [Word8] -> Bool
verifyBech32checksum hrp dat = bech32Polymod (bech32HRPExpand hrp <> dat) == 1

-- Top-level functions

checksum :: ByteString -> Checksum
checksum = Checksum . BS.pack . bech32checksum mempty . BS.unpack

verifyChecksum :: ByteString -> Checksum -> Bool
verifyChecksum protectedData =
    verifyBech32checksum mempty . BS.unpack . mappend protectedData . fromChecksum
