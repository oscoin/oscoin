{-# LANGUAGE NoStrictData #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Crypto.Hash.Mock
    ( MockHash(..)
    , Hash(..)
    ) where

import           Oscoin.Prelude hiding (length)

import           Oscoin.Crypto
import           Oscoin.Crypto.Hash

import           Codec.Serialise
import           Crypto.Data.Auth.Tree.Internal (MerkleHash(..))
import           Data.Aeson hiding (decode, encode)
import           Data.Aeson.Types (typeMismatch)
import           Data.ByteArray (ByteArray, ByteArrayAccess(..), convert)
import           Data.ByteArray.Orphans ()
import qualified Data.ByteString as BS
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Formatting as F
import           Formatting.Buildable
import           Text.Read (readMaybe)
import qualified Text.Show as Show
import           Web.HttpApiData (FromHttpApiData(..))

-- For our mock implementation we use a fast, non-cryptographic hash
-- See: https://cyan4973.github.io/xxHash/
import           Data.Digest.XXHash.FFI


data MockHash = MockHash deriving (Eq, Ord, Show)

instance HasHashing MockCrypto where
    type HashAlgorithm MockCrypto = MockHash

    -- Nb.: `NoStrictData` due to https://ghc.haskell.org/trac/ghc/ticket/16141
    newtype Hash MockCrypto = XxHash { fromMockHash :: Word64 }
        deriving (Eq, Ord)

    fromByteArray = XxHash . hashByteArray

    hashAlgorithm = MockHash
    zeroHash = XxHash minBound
    shortHash = BS.take 7
              . BaseN.encodedBytes
              . BaseN.encodeBase58
              . toS
              . BL.toLazyByteString
              . BL.word64LE
              . fromMockHash

-- | Hash anything which is an instance of 'ByteArray' into a 'Word64' using
-- the xxhash.
hashByteArray :: ByteArray ba => ba -> Word64
hashByteArray ba =
    let !(b :: ByteString) = convert ba
    in b `deepseq` xxh64 b 0

{------------------------------------------------------------------------------
  More-or-less-dubious instances
------------------------------------------------------------------------------}

instance Show.Show (Hash MockCrypto) where
    show = show . shortHash

instance Serialise (Hash MockCrypto) where
    encode (XxHash w32) = encode w32
    decode = XxHash <$> decode

instance ByteArrayAccess (Hash MockCrypto) where
    length (XxHash w64) = length
                        . LBS.toStrict
                        . BL.toLazyByteString
                        . BL.word64LE
                        $ w64
    withByteArray (XxHash w64) =
        withByteArray (LBS.toStrict . BL.toLazyByteString . BL.word64LE $ w64)

instance Hashable MockCrypto ByteString where
    hash = toHashed . XxHash . hashByteArray

instance Hashable MockCrypto LByteString where
    hash = toHashed . XxHash . hashByteArray @ByteString . toS

instance Hashable MockCrypto Word8 where
    hash = toHashed . XxHash . hashByteArray . BS.singleton

instance Hashable MockCrypto Text where
    hash = toHashed . XxHash . hashByteArray . encodeUtf8

instance H.Hashable (Hash MockCrypto) where
    hashWithSalt salt = H.hashWithSalt salt . shortHash

instance ToJSON (Hash MockCrypto) where
    toJSON (XxHash w32)     = toJSON . T.pack . show $ w32
    toEncoding (XxHash w32) = toEncoding . T.pack . show $ w32

instance FromJSON (Hash MockCrypto) where
    parseJSON = withText "Hash" $ \t ->
        case readMaybe . T.unpack $ t of
          Just w32 -> pure $ XxHash w32
          Nothing  -> typeMismatch "parseJSON for (Hash MockCrypto) failed" (toJSON t)

instance FromHttpApiData (Hash MockCrypto) where
    parseQueryParam t =
        case readMaybe . T.unpack $ t of
          Just w32 -> pure $ XxHash w32
          Nothing  -> Left $ "FromHttpApiData (Hash MockCrypto) failed for " <> t

instance MerkleHash (Hash MockCrypto) where
    emptyHash                            = XxHash minBound
    hashLeaf k v                         = XxHash $ hashByteArray @ByteString (convert k <> convert v)
    concatHashes (XxHash d1) (XxHash d2) = XxHash (d1 + d2)

instance Buildable (Hash MockCrypto) where
    build (XxHash w32) = F.bprint F.build w32

instance Sql.ToField (Hash MockCrypto) where
    toField (XxHash w32) = Sql.SQLText . T.pack . show $ w32

instance Sql.FromField (Hash MockCrypto) where
    fromField f =
        case Sql.fieldData f of
            Sql.SQLText t ->
                maybe sqlErr (Sql.Ok . XxHash) (readMaybe . T.unpack $ t)
            _ ->
                sqlErr
      where
        sqlErr = Sql.returnError Sql.ConversionFailed f
                 "couldn't convert from Word32 to XxHash"
