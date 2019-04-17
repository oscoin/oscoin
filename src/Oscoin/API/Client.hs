module Oscoin.API.Client
    ( MonadClient(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Types
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Data.Tx (Tx, TxPayload)

class Monad m => MonadClient c m where
    submitTransaction :: Tx c -> m (Result (TxSubmitResponse c (Tx c)))

    -- | Returns an error result if a transaction with the given hash
    -- was not found.
    getTransaction :: Hashed c (Tx c) -> m (Result (TxLookupResponse c))

    -- | Returns an error result if a value with the given key was not
    -- found.
    getState :: Proxy c -> [Text] -> m (Result (TxPayload c (Tx c)))
