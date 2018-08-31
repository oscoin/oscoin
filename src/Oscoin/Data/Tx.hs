module Oscoin.Data.Tx where

import           Oscoin.Prelude

import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad
import qualified Radicle as Rad

import           Codec.Serialise
import           Crypto.Random.Types (MonadRandom(..))
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Binary

data Tx msg = Tx
    { txMessage :: Signed msg
    , txPubKey  :: Hashed PublicKey
    , txChainId :: Word16
    , txNonce   :: Word32
    , txContext :: BlockHash
    } deriving (Show, Eq, Ord, Generic, Functor)

instance Binary msg => Binary (Tx msg)

instance Binary msg => Hashable (Tx msg) where
    hash = hashBinary

instance ToJSON (Tx ByteString) where
    toJSON Tx{..} =
        object [ "msg"     .= toJSON txMessage
               , "pubkey"  .= toJSON txPubKey
               , "chainId" .= toJSON txChainId
               , "nonce"   .= toJSON txNonce
               , "ctx"     .= toJSON txContext
               ]

instance FromJSON (Tx ByteString) where
    parseJSON = withObject "Tx" $ \o -> do
        txMessage <- o .: "msg"
        txPubKey  <- o .: "pubkey"
        txChainId <- o .: "chainId"
        txNonce   <- o .: "nonce"
        txContext <- o .: "ctx"
        pure Tx{..}

instance Serialise msg => Serialise (Tx msg)

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
createTx :: (Serialise msg, MonadRandom m) => (PublicKey, PrivateKey) -> msg -> m (Tx ByteString)
createTx (pk, sk) val = do
    sval <- sign sk (LBS.toStrict $ serialise val)
    pure $ mkTx sval (hash pk)
