module Oscoin.Data.Tx where

import           Oscoin.Prelude

import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad

import           Codec.Serialise
import           Crypto.Random.Types (MonadRandom(..))
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Binary
import           Data.Text.Prettyprint.Doc

data Tx msg = Tx
    { txMessage :: Signed msg
    , txPubKey  :: Hashed PublicKey
    , txChainId :: Word16
    , txNonce   :: Word32
    , txContext :: BlockHash
    } deriving (Show, Eq, Ord, Generic, Functor)

instance Binary msg => Binary (Tx msg)

instance Serialise msg => Hashable (Tx msg) where
    hash = hashSerial

instance Serialise msg => Serialise (Tx msg)

instance Serialise msg => ToJSON (Tx msg) where
    toJSON Tx{..} =
        object [ "msg"     .= toJSON (LBS.toStrict . serialise <$> txMessage)
               , "pubkey"  .= toJSON txPubKey
               , "chainId" .= toJSON txChainId
               , "nonce"   .= toJSON txNonce
               , "ctx"     .= toJSON txContext
               ]

instance Serialise msg => FromJSON (Tx msg) where
    parseJSON = withObject "Tx" $ \o -> do
        smsg <- o .: "msg"
        let txMessage = deserialise . LBS.fromStrict <$> smsg

        txPubKey  <- o .: "pubkey"
        txChainId <- o .: "chainId"
        txNonce   <- o .: "nonce"
        txContext <- o .: "ctx"
        pure Tx{..}

instance Pretty msg => Pretty (Tx msg) where
    pretty Tx{txMessage} = pretty txMessage

mkTx :: Signed msg -> Hashed PublicKey -> Tx msg
mkTx sm p = Tx
    { txMessage = sm
    , txPubKey  = p
    , txChainId = 0
    , txNonce   = 0
    , txContext = toHashed zeroHash
    }

-- | Convert a 'Tx' to a Radicle 'Program'.
toProgram :: Tx Rad.Value -> Rad.Program
toProgram Tx{..} =
    Rad.Program
        { Rad.progValue   = unsign txMessage
        , Rad.progAuthor  = txPubKey
        , Rad.progChainId = txChainId
        , Rad.progNonce   = txNonce
        }

-- | Create a 'Tx' from a 'Serialise' message and key pair.
createTx :: (Serialise msg, MonadRandom m) => (PublicKey, PrivateKey) -> msg -> m (Tx msg)
createTx (pk, sk) val = do
    sval <- sign sk val
    pure $ mkTx sval (hash pk)
