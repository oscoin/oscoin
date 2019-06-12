{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Data.Tx where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey
import           Oscoin.Data.Tx.Abstract

import           Codec.Serialise
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Monad.Fail (fail)
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.Map as Map

import           Oscoin.Data.Query


type instance TxOutput c (Tx c) = DummyPayload
type instance TxState c (Tx c) = LegacyTxState

newtype LegacyTxState = LegacyTxState (Map ByteString ByteString)
    deriving (Show, Eq, Semigroup, Monoid)

emptyState :: LegacyTxState
emptyState = LegacyTxState mempty

instance HasHashing c => Hashable c LegacyTxState where
    hash (LegacyTxState st) = toHashed . fromHashed . hashSerial $ st

instance Query LegacyTxState where
    query key (LegacyTxState st) = Map.lookup key st

newtype DummyPayload = DummyPayload ByteString
    deriving (Show, Eq, Ord, Serialise, ByteArrayAccess)

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


evaluateBlock :: Evaluator c LegacyTxState (Tx c) DummyPayload
evaluateBlock _beneficiary txs st = first reverse $ foldl' go ([], st) txs
  where
    go (output, st') tx = (txMessageContent tx : output, st')
