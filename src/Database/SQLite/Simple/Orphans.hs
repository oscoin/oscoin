{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.SQLite.Simple.Orphans where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.RadicleTx

import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as LBS
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Ok (Ok(..))

toFieldSerial :: CBOR.Serialise a => a -> SQLData
toFieldSerial a = SQLBlob . LBS.toStrict $ CBOR.serialise a

fromFieldSerial :: (Typeable a, CBOR.Serialise a) => Field -> Ok a
fromFieldSerial f =
    case fieldData f of
        SQLBlob bs -> Ok $ CBOR.deserialise (LBS.fromStrict bs)
        _          -> returnError ConversionFailed f "couldn't convert from CBOR"

instance FromRow (BlockHeader ()) where
    fromRow = BlockHeader
        <$> field
        <*> field
        <*> pure ()
        <*> field
        <*> field
        <*> field

instance FromRow RadTx where
    fromRow = Tx
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field

instance ToRow RadTx where
    toRow tx@Tx{..} =
        [ toField (Crypto.hash tx), toField txMessage, toField txPubKey
        , toField txChainId, toField txNonce, toField txContext ]

instance (CBOR.Serialise msg, Typeable msg) => FromField (Crypto.Signed msg) where
    fromField = fromFieldSerial

instance (CBOR.Serialise msg) => ToField (Crypto.Signed msg) where
    toField = toFieldSerial

instance FromField Crypto.PublicKey where
    fromField = fromFieldSerial

instance ToField Crypto.PublicKey where
    toField = toFieldSerial
