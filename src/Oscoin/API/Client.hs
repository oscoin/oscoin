-- | Defines abstract interface for a Node API client.
module Oscoin.API.Client
    ( Client(..)
    , hoistClient
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Types
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Data.Tx

data Client c m = Client
    { submitTransaction :: Tx c -> m (Result (TxSubmitResponse c (Tx c)))

    -- | Returns an error result if a transaction with the given hash
    -- was not found.
    , getTransaction :: Hashed c (Tx c) -> m (Result (TxLookupResponse c (Tx c)))

    , getState :: ByteString -> m (Maybe ByteString)
    }


hoistClient :: (forall a. m a -> n a) -> Client c m -> Client c n
hoistClient natTrsf client = Client
    { submitTransaction = natTrsf . submitTransaction client
    , getTransaction = natTrsf . getTransaction client
    , getState = natTrsf . getState client
    }
