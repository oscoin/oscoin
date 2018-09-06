module Oscoin.API.HTTP.Response where

import           Oscoin.Prelude

import           Oscoin.API.HTTP.Internal (ApiTx)
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)

import           Data.Aeson (FromJSON, ToJSON)
import           Codec.Serialise (Serialise)

-- | Response type for the GET /transactions/:hash API endpoint.
data GetTxResponse = GetTxResponse
    { txHash          :: Hashed ApiTx
    , txBlockHash     :: Maybe BlockHash
    , txConfirmations :: Word64
    , txPayload       :: ApiTx
    } deriving (Show, Eq, Generic)

instance ToJSON GetTxResponse
instance FromJSON GetTxResponse
instance Serialise GetTxResponse
