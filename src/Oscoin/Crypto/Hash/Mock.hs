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

    fromByteArray = FnvHash . hashByteArray

    hashAlgorithm = MockHash
    zeroHash = FnvHash (FnvHash64 minBound)
    shortHash (FnvHash (FnvHash64 w64)) = BS.take 7
                                        . BaseN.encodedBytes
                                        . BaseN.encodeBase58
                                        . toS
                                        . BL.toLazyByteString
                                        . BL.word64LE
                                        $ w64

-- | Hash anything which is an instance of 'ByteArray' into a 'Word64' using
-- the xxhash.
hashByteArray :: ByteArray ba => ba -> FnvHash64
hashByteArray = fnv1a_64Hash

{------------------------------------------------------------------------------
  More-or-less-dubious instances
------------------------------------------------------------------------------}

instance Show.Show (Hash MockCrypto) where
    show = show . shortHash

instance Serialise (Hash MockCrypto) where
    encode (FnvHash (FnvHash64 w64)) = encode w64
    decode = FnvHash . FnvHash64 <$> decode

instance ByteArrayAccess (Hash MockCrypto) where
    length (FnvHash (FnvHash64 w64)) = length
                                    . LBS.toStrict
                                    . BL.toLazyByteString
                                    . BL.word64LE
                                    $ w64
    withByteArray (FnvHash (FnvHash64 w64)) =
        withByteArray (LBS.toStrict . BL.toLazyByteString . BL.word64LE $ w64)

instance Hashable MockCrypto ByteString where
    hash = toHashed . FnvHash . hashByteArray

instance Hashable MockCrypto LByteString where
    hash = toHashed . FnvHash . hashByteArray @ByteString . toS

instance Hashable MockCrypto Word8 where
    hash = toHashed . FnvHash . hashByteArray . BS.singleton

instance Hashable MockCrypto Text where
    hash = toHashed . FnvHash . hashByteArray . encodeUtf8

instance H.Hashable (Hash MockCrypto) where
    hashWithSalt salt = H.hashWithSalt salt . shortHash

instance ToJSON (Hash MockCrypto) where
    toJSON (FnvHash (FnvHash64 w64)) = toJSON . T.pack . show $ w64
    toEncoding (FnvHash (FnvHash64 w64)) = toEncoding . T.pack . show $ w64

instance FromJSON (Hash MockCrypto) where
    parseJSON = withText "Hash" $ \t ->
        case readMaybe . T.unpack $ t of
          Just w64 -> pure $ FnvHash (FnvHash64 w64)
          Nothing  -> typeMismatch "parseJSON for (Hash MockCrypto) failed" (toJSON t)

instance FromHttpApiData (Hash MockCrypto) where
    parseQueryParam t =
        case readMaybe . T.unpack $ t of
          Just w64 -> pure $ FnvHash (FnvHash64 w64)
          Nothing  -> Left $ "FromHttpApiData (Hash MockCrypto) failed for " <> t

instance MerkleHash (Hash MockCrypto) where
    emptyHash                            =
        FnvHash (FnvHash64 minBound)
    hashLeaf k v                         =
        FnvHash $ hashByteArray @ByteString (convert k <> convert v)
    concatHashes (FnvHash (FnvHash64 d1)) (FnvHash (FnvHash64 d2)) =
        FnvHash $ FnvHash64 (d1 + d2)

instance Buildable (Hash MockCrypto) where
    build (FnvHash (FnvHash64 w64)) = F.bprint F.build w64

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
