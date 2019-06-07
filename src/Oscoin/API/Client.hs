-- | Defines abstract interface for a Node API client.
module Oscoin.API.Client
    ( Client(..)
    , hoistClient
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Types

data Client c tx m = Client
    { submitTransaction :: tx -> m (Result (TxSubmitResponse c tx))
    , getState          :: ByteString -> m (Maybe ByteString)
    }


hoistClient :: (forall a. m a -> n a) -> Client c tx m -> Client c tx n
hoistClient natTrsf client = Client
    { submitTransaction = natTrsf . submitTransaction client
    , getState = natTrsf . getState client
    }
