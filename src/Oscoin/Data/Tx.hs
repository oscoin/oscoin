module Oscoin.Data.Tx where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash (zeroHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey

import           Codec.Serialise
import           Crypto.Random.Types (MonadRandom(..))
import           Data.Aeson
import           Data.Text.Prettyprint.Doc

import qualified Radicle.Extended as Rad

data Tx msg = Tx
    { txMessage :: Signed msg
    , txPubKey  :: PublicKey
    , txChainId :: Word16
    , txNonce   :: Word32
    , txContext :: BlockHash
    } deriving (Show, Eq, Ord, Generic, Functor)

instance Serialise msg => Crypto.Hashable (Tx msg) where
    hash = Crypto.hashSerial

instance Serialise msg => Serialise (Tx msg)

instance ToJSON (Tx Rad.Value) where
    toJSON tx@Tx{..} =
        object [ "hash"    .= toJSON (Crypto.hash tx)
               , "message" .= toJSON (map Rad.prettyValue txMessage)
               , "pubkey"  .= toJSON txPubKey
               , "chainId" .= toJSON txChainId
               , "nonce"   .= toJSON txNonce
               , "ctx"     .= toJSON txContext
               ]

instance FromJSON (Tx Rad.Value) where
    parseJSON = withObject "Tx" $ \o -> do
        signedJson <- o .: "message"
        txMessage <- for signedJson Rad.parseFromJson
        txPubKey  <- o .: "pubkey"
        txChainId <- o .: "chainId"
        txNonce   <- o .: "nonce"
        txContext <- o .: "ctx"
        pure Tx{..}

instance Pretty msg => Pretty (Tx msg) where
    pretty Tx{txMessage} = pretty txMessage

mkTx :: Signed msg -> PublicKey -> Tx msg
mkTx sm pk = Tx
    { txMessage = sm
    , txPubKey  = pk
    , txChainId = 0
    , txNonce   = 0
    , txContext = zeroHash
    }


txMessageContent :: Tx a -> a
txMessageContent = sigMessage . txMessage

-- | Create a 'Tx' from a 'Serialise' message and key pair.
createTx :: (Serialise msg, MonadRandom m) => (PublicKey, PrivateKey) -> msg -> m (Tx msg)
createTx (pk, sk) val = do
    sval <- sign sk val
    pure $ mkTx sval pk

verifyTx :: Serialise msg => Tx msg -> Bool
verifyTx Tx{..} = verify txPubKey txMessage
