{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Crypto.Hash.RealWorld where

import           Oscoin.Prelude
import qualified Prelude

import           Codec.Serialise
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import qualified Codec.Serialise.Multihash as Multihash.CBOR
import           Control.Monad.Fail (fail)
import           Crypto.Data.Auth.Tree.Class (MerkleHash(..))
import qualified Crypto.Data.Auth.Tree.Cryptonite as Cryptonite
import           Crypto.Hash (Blake2b_256(..), Digest, RIPEMD160(..))
import qualified Crypto.Hash as Crypto
import           Data.Aeson (FromJSON(..), ToJSON(..), withText)
import           Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import           Data.Maybe (fromJust)
import           Data.Multihash (Multihashable)
import qualified Data.Multihash as Multihash
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Formatting as F
import           Formatting.Buildable (Buildable(..))
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Hash
import qualified Text.Show as Show
import           Web.HttpApiData (FromHttpApiData(..))

{------------------------------------------------------------------------------
  Crypto-specific instances
------------------------------------------------------------------------------}

instance HasHashing Crypto where
    type HashAlgorithm Crypto = Blake2b_256

    -- | A 'Digest' obtained by some hash algorithm @(HashAlgorithm Crypto)@.
    --
    -- Textual representations use base-58 encoding. Serialisation and
    -- deserialisation via "Crypto.Hash.Multi".
    --
    -- Nb.: We need to `NoStrictData` this module due to
    -- https://ghc.haskell.org/trac/ghc/ticket/16141
    newtype Hash Crypto =
        Hash { fromHash :: Digest (HashAlgorithm Crypto) }
        deriving (Eq, Ord, ByteArrayAccess)

    newtype ShortHash Crypto =
        ShortHash { fromShortHash :: Digest RIPEMD160 }
        deriving (Eq, Ord, ByteArrayAccess)

    hashByteArray = Hash . Crypto.hash

    hashAlgorithm = Blake2b_256

    parseShortHash t = do
        t' <- T.stripPrefix "0x" t
        case BaseN.decodeAtBaseEither BaseN.Base16 (encodeUtf8 t') of
            Left _  -> Nothing
            Right h -> ShortHash <$> Crypto.digestFromByteString h

    toShortHash h = ShortHash
                  . Crypto.hash @ByteString @RIPEMD160
                  . ByteArray.convert @(Crypto.Digest (HashAlgorithm Crypto)) @ByteString
                  . fromHash
                  $ h

    zeroHash = Hash . fromJust $
        Crypto.digestFromByteString @(HashAlgorithm Crypto) @ByteString
            (ByteArray.zero (hashDigestSize_ (Proxy @(HashAlgorithm Crypto))))

    zeroShortHash = ShortHash . fromJust $
        Crypto.digestFromByteString @RIPEMD160 @ByteString
            (ByteArray.zero (hashDigestSize_ (Proxy @RIPEMD160)))

    compactHash = compactHashRealWorld

{------------------------------------------------------------------------------
  Instances galore
-------------------------------------------------------------------------------}

instance Hashable Crypto Text where
    hash = toHashed . Hash . Crypto.hash . encodeUtf8

instance Hashable Crypto ByteString where
    hash = toHashed . Hash . Crypto.hash

instance Hashable Crypto LByteString where
    hash = toHashed . fromHashed . hash . LBS.toStrict

instance Hashable Crypto Word8 where
    hash = toHashed . Hash . Crypto.hash . BS.singleton

instance (ByteArrayAccess a) => Hashable Crypto (Maybe a) where
    hash (Just x) = toHashed . Hash $ Crypto.hash x
    hash Nothing  = toHashed $ zeroHash

{------------------------------------------------------------------------------
  Serialisation instances
------------------------------------------------------------------------------}

instance ToJSON (Hash Crypto) where
    toJSON     = toJSON . F.sformat formatHash
    toEncoding = toEncoding . F.sformat formatHash

instance FromJSON (Hash Crypto) where
    parseJSON = withText "Hash" $
        either fail pure
            . second Hash . decodeAtBase BaseN.Base58btc . encodeUtf8

instance ToJSON (ShortHash Crypto) where
    toJSON     = toJSON . T.pack . show
    toEncoding = toEncoding . T.pack . show

instance FromJSON (ShortHash Crypto) where
    parseJSON = withText "ShortHash" $ \t ->
        case parseShortHash t of
            Nothing -> fail . toS $
                "failed to parse 'ShortHash Crypto' from input: '" <> t <> "'"
            Just h  -> pure h

instance Sql.ToField (Hash Crypto) where
    toField = Sql.SQLText . F.sformat formatHash

instance Sql.FromField (Hash Crypto) where
    fromField f =
        case Sql.fieldData f of
            Sql.SQLText t ->
                either (const sqlErr) (Sql.Ok . Hash)
                       (decodeAtBase BaseN.Base58btc $ encodeUtf8 t)
            _ ->
                sqlErr
      where
        sqlErr = Sql.returnError Sql.ConversionFailed f
            "couldn't convert from Base58"

instance Multihashable (HashAlgorithm Crypto) => Serialise (Hash Crypto) where
    encode = Multihash.CBOR.encode . fromHash
    decode = Hash <$> Multihash.CBOR.decode

instance Serialise (ShortHash Crypto) where
    encode = CBOR.encodeBytes . convert . fromShortHash
    decode = do
        mbDigest <- Crypto.digestFromByteString @RIPEMD160 @ByteString <$> CBOR.decode
        case mbDigest of
          Nothing  -> fail "error decoding ShortHash Crypto: not a valid digest."
          Just dig -> pure (ShortHash dig)

instance Multihashable (HashAlgorithm Crypto) => FromHttpApiData (Hash Crypto) where
    parseQueryParam =
        bimap T.pack Hash . decodeAtBase BaseN.Base58btc . encodeUtf8

-- FIXME: this should use multihash encoding and satisfy @read . show = id@
instance Show.Show (Hash Crypto) where
    show (Hash d) = C8.unpack
                  . BaseN.encodedBytes
                  . BaseN.encodeBase58btc
                  . convert
                  $ d

instance Show.Show (ShortHash Crypto) where
    show = T.unpack . F.sformat formatShortHash

instance H.Hashable (Hash Crypto) where
    hashWithSalt salt (Hash d) =
        let (bs :: ByteString) = ByteArray.convert d
        in H.hashWithSalt salt bs

instance Buildable (Hash Crypto) where
    build (Hash digest) =
        BaseN.encodedTextBuilder $
            encodeAtBase BaseN.Base58btc . Multihash.fromDigest $ digest

instance Buildable (ShortHash Crypto) where
    build (ShortHash digest) =
        BaseN.encodedTextBuilder $
            BaseN.encodeAtBase BaseN.Base16 (ByteArray.convert digest)

instance MerkleHash (Hash Crypto) where
    emptyHash    = Hash Cryptonite.emptyHash
    hashLeaf k v = Hash (Cryptonite.hashLeaf k v)
    concatHashes (Hash d1) (Hash d2) = Hash (Cryptonite.concatHashes d1 d2)

{------------------------------------------------------------------------------
  Utility functions
-------------------------------------------------------------------------------}

-- | Convert a 'Digest' to a 'Hash''.
toHash :: Digest (HashAlgorithm Crypto) -> Hash Crypto
toHash = Hash

hashDigestSize_ :: forall proxy a. Crypto.HashAlgorithm a => proxy a -> Int
hashDigestSize_ _ = Crypto.hashDigestSize (Prelude.undefined :: a)

hashBlockSize_ :: forall proxy a. Crypto.HashAlgorithm a => proxy a -> Int
hashBlockSize_ _ = Crypto.hashBlockSize (Prelude.undefined :: a)

-- | The first 7 bytes of the base-58 encoded hash.
compactHashRealWorld :: Hash Crypto -> ByteString
compactHashRealWorld (Hash d) =
      BS.take 7
    . BaseN.encodedBytes
    . BaseN.encodeBase58btc
    . convert
    $ d
