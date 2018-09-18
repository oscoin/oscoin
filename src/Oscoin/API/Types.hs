module Oscoin.API.Types
    ( RadTx
    , Result(..)
    , TxLookupResponse(..)
    , isOk
    , isErr
    , resultToEither
    , Receipt
    , Key
    , Query(..)
    ) where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Data.Query (Query(..))
import           Oscoin.Data.Tx (Tx)
import           Oscoin.Node (Receipt)
import           Oscoin.State.Tree (Key)
import qualified Radicle as Rad

import qualified Data.Aeson as Aeson
import qualified Codec.Serialise as Serial

-- | The type of a block transaction in the API.
type RadTx = Tx Rad.Value

data Result a =
      Ok  a
    | Err Text
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (Result a)
instance Aeson.FromJSON a => Aeson.FromJSON (Result a)
instance Serial.Serialise a => Serial.Serialise (Result a)

isOk :: Result a -> Bool
isOk (Ok _) = True
isOk _      = False

isErr :: Result a -> Bool
isErr = not . isOk

resultToEither :: Result a -> Either Text a
resultToEither (Ok a ) = Right a
resultToEither (Err t) = Left t

-- | Response type of a transaction lookup API operation.
data TxLookupResponse = TxLookupResponse
    { txHash          :: Hashed RadTx
    , txBlockHash     :: Maybe BlockHash
    , txConfirmations :: Word64
    , txPayload       :: RadTx
    } deriving (Show, Eq, Generic)

instance Aeson.ToJSON TxLookupResponse
instance Aeson.FromJSON TxLookupResponse
instance Serial.Serialise TxLookupResponse
