{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.SQLite.Simple.Orphans where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto

import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as LBS
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow (fieldWith)
import           Database.SQLite.Simple.Ok (Ok(..))
import           Database.SQLite.Simple.ToField

toFieldSerial :: CBOR.Serialise a => a -> SQLData
toFieldSerial a = SQLBlob . LBS.toStrict $ CBOR.serialise a

fromFieldSerial :: (Typeable a, CBOR.Serialise a) => Field -> Ok a
fromFieldSerial f =
    case fieldData f of
        SQLBlob bs -> Ok $ CBOR.deserialise (LBS.fromStrict bs)
        _          -> returnError ConversionFailed f "couldn't convert from CBOR"

instance ( Crypto.HasHashing c
         , FromField (Crypto.Hash c)
         , FromField s
         ) => FromRow (BlockHeader c s) where
    fromRow = BlockHeader
        <$> fieldWith fromPrevHashField
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field

-- | Convert a SQL parent hash field to a 'Crypto.Hash'. The SQL value is
-- 'SQLNull' in the case of the genesis block.
fromPrevHashField
    :: ( FromField (Crypto.Hash c)
       , Crypto.HasHashing c
       )
    => Field
    -> Ok (Crypto.Hash c)
fromPrevHashField f =
    case fieldData f of
        SQLNull -> Ok Crypto.zeroHash
        _       -> fromField f

instance ( CBOR.Serialise (Crypto.Signature c)
         , CBOR.Serialise msg
         , Typeable c
         , Typeable msg
         ) => FromField (Crypto.Signed c msg) where
    fromField = fromFieldSerial

instance ( CBOR.Serialise msg
         , CBOR.Serialise (Crypto.Signature c)
         ) => ToField (Crypto.Signed c msg) where
    toField = toFieldSerial

instance (CBOR.Serialise (Crypto.PublicKey c), Typeable c) => FromField (Crypto.PublicKey c) where
    fromField = fromFieldSerial

instance CBOR.Serialise (Crypto.PublicKey c) => ToField (Crypto.PublicKey c) where
    toField = toFieldSerial

instance ToField s => ToField (Sealed c s) where
    toField (SealedWith s) = toField s

instance FromField s => FromField (Sealed c s) where
    fromField = map SealedWith . fromField
