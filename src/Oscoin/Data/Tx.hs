{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Data.Tx where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash (zeroHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey

import           Codec.Serialise
import           Crypto.Random.Types (MonadRandom(..))
import           Data.Aeson
import           Data.ByteArray (ByteArrayAccess)
import           Data.Text.Prettyprint.Doc

import qualified Radicle.Extended as Rad

data Tx c msg = Tx
    { txMessage :: Signed c msg
    , txPubKey  :: PK c
    , txChainId :: Word16
    , txNonce   :: Word32
    , txContext :: BlockHash c
    } deriving (Generic, Functor)

deriving instance ( Show (Signature c)
                  , Show (BlockHash c)
                  , Crypto.HasHashing c
                  , Show (PK c)
                  , Show msg
                  ) => Show (Tx c msg)
deriving instance ( Eq (Signature c)
                  , Eq (BlockHash c)
                  , Eq (PK c)
                  , Eq msg
                  )  => Eq (Tx c msg)
deriving instance ( Ord (Signature c)
                  , Ord (BlockHash c)
                  , Ord (PK c)
                  , Ord msg
                  ) => Ord (Tx c msg)

instance ( Crypto.HasHashing c
         , Serialise (PK c)
         , Serialise (BlockHash c)
         , Serialise msg
         , Serialise (Signature c)
         ) => Crypto.Hashable c (Tx c msg) where
    hash = Crypto.hashSerial

instance ( Serialise (PK c)
         , Serialise (BlockHash c)
         , Serialise msg
         , Serialise (Signature c)
         ) => Serialise (Tx c msg)

instance ( Crypto.HasHashing c
         , Crypto.Hashable c (Tx c Rad.Value)
         , ToJSON (PK c)
         , ToJSON (Crypto.Hash c)
         , ToJSON (Signature c)
         ) => ToJSON (Tx c Rad.Value) where
    toJSON tx@Tx{..} =
        let (h :: Crypto.Hashed c (Tx c Rad.Value)) = Crypto.hash tx
        in object [ "hash"    .= toJSON h
                  , "message" .= toJSON (map Rad.prettyValue txMessage)
                  , "pubkey"  .= toJSON txPubKey
                  , "chainId" .= toJSON txChainId
                  , "nonce"   .= toJSON txNonce
                  , "ctx"     .= toJSON txContext
                  ]

instance ( FromJSON (BlockHash c)
         , FromJSON (PK c)
         , FromJSON (Signature c)
         ) => FromJSON (Tx c Rad.Value) where
    parseJSON = withObject "Tx" $ \o -> do
        signedJson <- o .: "message"
        txMessage <- for signedJson Rad.parseFromJson
        txPubKey  <- o .: "pubkey"
        txChainId <- o .: "chainId"
        txNonce   <- o .: "nonce"
        txContext <- o .: "ctx"
        pure Tx{..}

instance Pretty msg => Pretty (Tx c msg) where
    pretty Tx{txMessage} = pretty txMessage

mkTx :: Crypto.HasHashing c => Signed c msg -> PK c -> Tx c msg
mkTx sm pk = Tx
    { txMessage = sm
    , txPubKey  = pk
    , txChainId = 0
    , txNonce   = 0
    , txContext = zeroHash
    }


txMessageContent :: Tx c a -> a
txMessageContent = sigMessage . txMessage

-- | Create a 'Tx' from a 'Serialise' message and key pair.
createTx
    :: ( HasDigitalSignature c
       , Crypto.HasHashing c
       , MonadRandom m
       , ByteArrayAccess msg
       )
    => KeyPair c
    -> msg
    -> m (Tx c msg)
createTx (pk, sk) val = do
    sval <- sign sk val
    pure $ mkTx sval pk

verifyTx
    :: ( HasDigitalSignature c
       , ByteArrayAccess msg
       ) => Tx c msg -> Bool
verifyTx Tx{..} = verify txPubKey txMessage
