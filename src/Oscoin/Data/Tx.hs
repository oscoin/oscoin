module Oscoin.Data.Tx where

import           Oscoin.Prelude

import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad

import           Codec.Serialise
import           Data.Aeson
import           Data.Binary

data Tx msg = Tx
    { txMessage :: Signed msg
    , txPubKey  :: Hashed PublicKey
    , txChainId :: Word16
    , txNonce   :: Word32
    , txContext :: BlockHash
    } deriving (Show, Eq, Ord, Generic)

instance Binary msg => Binary (Tx msg)

instance Binary msg => Hashable (Tx msg) where
    hash = hashBinary

instance ToJSON msg => ToJSON (Tx msg) where
    toJSON Tx{..} =
        object [ "msg"     .= toJSON txMessage
               , "pubkey"  .= toJSON txPubKey
               , "chainId" .= toJSON txChainId
               , "nonce"   .= toJSON txNonce
               , "ctx"     .= toJSON txContext
               ]

instance FromJSON msg => FromJSON (Tx msg) where
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
toProgram :: Tx ByteString -> Rad.Program
toProgram Tx{..} =
    Rad.Program
        { Rad.progSource  = unsign txMessage
        , Rad.progAuthor  = txPubKey
        , Rad.progChainId = txChainId
        , Rad.progNonce   = txNonce
        }
