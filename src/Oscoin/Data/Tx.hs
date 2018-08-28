module Oscoin.Data.Tx where

import           Oscoin.Prelude

import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Blockchain.Block (BlockHash)

import           Codec.Serialise
import           Data.Aeson
import           Data.Binary

data Tx msg = Tx
    { txMessage :: Signed msg
    , txPubKey  :: Hashed PublicKey
    , txChainId :: Word16
    , txNonce   :: Word32
    , txContext :: BlockHash
    } deriving (Show, Generic)

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
