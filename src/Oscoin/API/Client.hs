module Oscoin.API.Client
    ( MonadClient(..)
    ) where

import           Oscoin.Prelude
import           Oscoin.API.Types
import           Oscoin.Crypto.Hash (Hashed)
import qualified Radicle as Rad

class Monad m => MonadClient m where
    submitTransaction :: RadTx -> m (Result (Receipt RadTx))

    -- | Returns an error result if a transaction with the given hash
    -- was not found.
    getTransaction :: Hashed RadTx -> m (Result RadTx)

    -- | Returns an error result if a value with the given key was not
    -- found.
    getState :: Key -> m (Result Rad.Value)
