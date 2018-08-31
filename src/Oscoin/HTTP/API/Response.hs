module Oscoin.HTTP.API.Response where

import           Oscoin.Prelude

import           Oscoin.HTTP.Internal (ApiTx)
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)

import           Data.Aeson (ToJSON)
import           Codec.Serialise (Serialise)

-- | Response type for the GET /transactions/:id API endpoint.
data GetTxResponse = GetTxResponse
    { txHash          :: Hashed ApiTx
    , txBlockHash     :: BlockHash
    , txConfirmations :: Word64
    , txPayload       :: ApiTx
    } deriving (Show, Eq, Generic)

instance ToJSON GetTxResponse
instance Serialise GetTxResponse
