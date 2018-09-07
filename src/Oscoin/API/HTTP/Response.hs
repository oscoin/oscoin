module Oscoin.API.HTTP.Response where

import           Oscoin.Prelude

import           Oscoin.API.Types (RadTx)
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)

import           Data.Aeson (FromJSON, ToJSON)
import           Codec.Serialise (Serialise)

-- | Response type for the GET /transactions/:hash API endpoint.
data GetTxResponse = GetTxResponse
    { txHash          :: Hashed RadTx
    , txBlockHash     :: Maybe BlockHash
    , txConfirmations :: Word64
    , txPayload       :: RadTx
    } deriving (Show, Eq, Generic)

instance ToJSON GetTxResponse
instance FromJSON GetTxResponse
instance Serialise GetTxResponse
