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
import           Crypto.Data.Auth.Tree.Class (MerkleHash(..))
import           Data.Aeson hiding (decode, encode)
import           Data.Aeson.Types (typeMismatch)
import           Data.ByteArray (ByteArrayAccess(..), convert)
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

-- For the mock crypto we use the FNV-1 hashing function.
-- Unfortunately using something faster like xxhash would generate
-- too many collisions.
import           Data.ByteArray.Hash (FnvHash64(..), fnv1a_64Hash)


data MockHash = MockHash deriving (Eq, Ord, Show)

instance HasHashing MockCrypto where
    type HashAlgorithm MockCrypto = MockHash

    -- Nb.: `NoStrictData` due to https://ghc.haskell.org/trac/ghc/ticket/16141
    newtype Hash MockCrypto = FnvHash { fromMockHash :: FnvHash64 }
        deriving (Eq, Ord)
    newtype ShortHash MockCrypto = ShortHash { fromShortHash :: Hash MockCrypto }
        deriving (Eq, Ord)

    -- | Hash anything which is an instance of 'ByteArray' into a 'Word64' using
    -- FNV-1.
    hashByteArray = FnvHash . fnv1a_64Hash

    hashAlgorithm = MockHash

    zeroHash = FnvHash (FnvHash64 minBound)

    zeroShortHash = ShortHash zeroHash

    toShortHash = ShortHash

    parseShortHash t =
        case readMaybe . T.unpack $ t of
          Just w64 -> pure $ ShortHash $ FnvHash (FnvHash64 w64)
          Nothing  -> Nothing

    compactHash (FnvHash (FnvHash64 w64)) = BS.take 7
                                          . BaseN.encodedBytes
                                          . BaseN.encodeBase58btc
                                          . toS
                                          . BL.toLazyByteString
                                          . BL.word64LE
                                          $ w64

{------------------------------------------------------------------------------
  More-or-less-dubious instances
------------------------------------------------------------------------------}

instance Show.Show (Hash MockCrypto) where
    show = show . compactHash

instance Show.Show (ShortHash MockCrypto) where
    show (ShortHash (FnvHash (FnvHash64 w64))) = show w64

instance Serialise (Hash MockCrypto) where
    encode (FnvHash (FnvHash64 w64)) = encode w64
    decode = FnvHash . FnvHash64 <$> decode

instance Serialise (ShortHash MockCrypto) where
    encode = encode . fromShortHash
    decode = ShortHash <$> decode

instance ByteArrayAccess (ShortHash MockCrypto) where
    length (ShortHash h)          = length h
    withByteArray (ShortHash h) f = withByteArray h f

instance ByteArrayAccess (Hash MockCrypto) where
    length (FnvHash (FnvHash64 w64)) = length
                                    . LBS.toStrict
                                    . BL.toLazyByteString
                                    . BL.word64LE
                                    $ w64
    withByteArray (FnvHash (FnvHash64 w64)) =
        withByteArray (LBS.toStrict . BL.toLazyByteString . BL.word64LE $ w64)

instance Hashable MockCrypto ByteString where
    hash = toHashed . hashByteArray

instance Hashable MockCrypto LByteString where
    hash = toHashed . hashByteArray . LBS.toStrict

instance Hashable MockCrypto Word8 where
    hash = toHashed . hashByteArray . BS.singleton

instance Hashable MockCrypto Text where
    hash = toHashed . hashByteArray . encodeUtf8

instance H.Hashable (Hash MockCrypto) where
    hashWithSalt salt = H.hashWithSalt salt . compactHash

instance ToJSON (Hash MockCrypto) where
    toJSON (FnvHash (FnvHash64 w64)) = toJSON . T.pack . show $ w64
    toEncoding (FnvHash (FnvHash64 w64)) = toEncoding . T.pack . show $ w64

instance FromJSON (Hash MockCrypto) where
    parseJSON = withText "Hash" $ \t ->
        case readMaybe . T.unpack $ t of
          Just w64 -> pure $ FnvHash (FnvHash64 w64)
          Nothing  -> typeMismatch "parseJSON for (Hash MockCrypto) failed" (toJSON t)

instance ToJSON (ShortHash MockCrypto) where
    toJSON (ShortHash h) = toJSON h
    toEncoding (ShortHash h) = toEncoding h

instance FromJSON (ShortHash MockCrypto) where
    parseJSON = withText "ShortHash" $ \t ->
        case readMaybe . T.unpack $ t of
          Just w64 -> pure $ ShortHash $ FnvHash (FnvHash64 w64)
          Nothing  -> typeMismatch "parseJSON for (ShortHash MockCrypto) failed" (toJSON t)

instance FromHttpApiData (Hash MockCrypto) where
    parseQueryParam t =
        case readMaybe . T.unpack $ t of
          Just w64 -> pure $ FnvHash (FnvHash64 w64)
          Nothing  -> Left $ "FromHttpApiData (Hash MockCrypto) failed for " <> t

instance MerkleHash (Hash MockCrypto) where
    emptyHash                            =
        FnvHash (FnvHash64 minBound)
    hashLeaf k v                         =
        hashByteArray ((convert k <> convert v) :: ByteString)
    concatHashes (FnvHash (FnvHash64 d1)) (FnvHash (FnvHash64 d2)) =
        FnvHash $ FnvHash64 (d1 + d2)

instance Buildable (Hash MockCrypto) where
    build (FnvHash (FnvHash64 w64)) = F.bprint F.build w64

instance Buildable (ShortHash MockCrypto) where
    build (ShortHash h) = build h

instance Sql.ToField (Hash MockCrypto) where
    toField (FnvHash (FnvHash64 w64)) = Sql.SQLText . T.pack . show $ w64

instance Sql.FromField (Hash MockCrypto) where
    fromField f =
        case Sql.fieldData f of
            Sql.SQLText t ->
                maybe sqlErr (Sql.Ok . FnvHash . FnvHash64) (readMaybe . T.unpack $ t)
            _ ->
                sqlErr
      where
        sqlErr = Sql.returnError Sql.ConversionFailed f
                 "couldn't convert from Word64 to FnvHash"
