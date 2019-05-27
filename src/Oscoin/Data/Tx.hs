{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Data.Tx where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey
import           Oscoin.Data.Tx.Abstract

import           Codec.Serialise
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Monad.Fail (fail)
import           Data.Aeson
import           Data.ByteArray (ByteArrayAccess)
import           Data.Coerce
import qualified Data.Map as Map

import qualified Oscoin.Data.OscoinTx as OscoinTx
import           Oscoin.Data.Query


type instance TxOutput c (Tx c) = OscoinTx.TxOutput
type instance TxState c (Tx c) = LegacyTxState

newtype LegacyTxState = LegacyTxState (Map ByteString ByteString)
    deriving (Show, Eq, Semigroup, Monoid)

instance HasHashing c => Hashable c LegacyTxState where
    hash (LegacyTxState st) = toHashed . fromHashed . hashSerial $ st

instance Query LegacyTxState where
    query key (LegacyTxState st) = Map.lookup key st

newtype DummyPayload = DummyPayload ByteString
    deriving (Show, Eq, Ord, Serialise, ByteArrayAccess)

-- These are bogus Aeson instances which serves just as a placeholder for
-- the times it takes to replace 'Tx' with the proper 'OscoinTx'.
instance ToJSON DummyPayload where
    toJSON = toJSON . decodeUtf8 . coerce

instance FromJSON DummyPayload where
    parseJSON o = DummyPayload . encodeUtf8 <$> parseJSON o

type Tx c = Tx' c DummyPayload

data Tx' c msg = Tx
    { txMessage :: Signed c msg
    , txPubKey  :: PublicKey c
    , txChainId :: Word16
    , txNonce   :: Word32
    , txContext :: BlockHash c
    } deriving (Generic, Functor)

deriving instance ( Show (Signature c)
                  , Show (BlockHash c)
                  , Crypto.HasHashing c
                  , Show (PublicKey c)
                  , Show msg
                  ) => Show (Tx' c msg)
deriving instance ( Eq (Signature c)
                  , Eq (BlockHash c)
                  , Eq (PublicKey c)
                  , Eq msg
                  )  => Eq (Tx' c msg)
deriving instance ( Ord (Signature c)
                  , Ord (BlockHash c)
                  , Ord (PublicKey c)
                  , Ord msg
                  ) => Ord (Tx' c msg)

instance ( Crypto.HasHashing c
         , Serialise (PublicKey c)
         , Serialise (BlockHash c)
         , Serialise msg
         , Serialise (Signature c)
         ) => Crypto.Hashable c (Tx' c msg) where
    hash = Crypto.hashSerial

instance ( Serialise (PublicKey c)
         , Serialise (BlockHash c)
         , Serialise msg
         , Serialise (Signature c)
         ) => Serialise (Tx' c msg) where
  encode Tx{..} =
       CBOR.encodeListLen 6
    <> CBOR.encodeWord 0
    <> CBOR.encode txMessage
    <> CBOR.encode txPubKey
    <> CBOR.encode txChainId
    <> CBOR.encode txNonce
    <> CBOR.encode txContext
  decode = do
      pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
      case pre of
          (6, 0) ->
              Tx <$> CBOR.decode
                 <*> CBOR.decode
                 <*> CBOR.decode
                 <*> CBOR.decode
                 <*> CBOR.decode
          e -> fail $ "Failed decoding Tx from CBOR: " ++ show e

instance ( Crypto.HasHashing c
         , Crypto.Hashable c (Tx' c msg)
         , ToJSON (PublicKey c)
         , ToJSON (Crypto.Hash c)
         , ToJSON (Signature c)
         , ToJSON msg
         ) => ToJSON (Tx' c msg) where
    toJSON tx@Tx{..} =
        let (h :: Crypto.Hashed c (Tx' c msg)) = Crypto.hash tx
        in object [ "hash"    .= toJSON h
                  , "message" .= toJSON txMessage
                  , "pubkey"  .= toJSON txPubKey
                  , "chainId" .= toJSON txChainId
                  , "nonce"   .= toJSON txNonce
                  , "ctx"     .= toJSON txContext
                  ]

instance ( FromJSON (BlockHash c)
         , FromJSON (PublicKey c)
         , FromJSON (Signature c)
         , FromJSON msg
         ) => FromJSON (Tx' c msg) where
    parseJSON = withObject "Tx" $ \o -> do
        txMessage <-  o .: "message"
        txPubKey  <- o .: "pubkey"
        txChainId <- o .: "chainId"
        txNonce   <- o .: "nonce"
        txContext <- o .: "ctx"
        pure Tx{..}

mkTx :: Crypto.HasHashing c => Signed c msg -> PublicKey c -> Tx' c msg
mkTx sm pk = Tx
    { txMessage = sm
    , txPubKey  = pk
    , txChainId = 0
    , txNonce   = 0
    , txContext = zeroHash
    }


txMessageContent :: Tx' c a -> a
txMessageContent = sigMessage . txMessage

data TxValidationError' = TxInvalidSignature

type instance TxValidationError c (Tx' c msg) = TxValidationError'

validateTx
    :: ( HasDigitalSignature c
       , ByteArrayAccess msg
       ) => Tx' c msg -> Either TxValidationError' ()
validateTx Tx{..} =
    if verify txPubKey txMessage
    then Right ()
    else Left TxInvalidSignature
