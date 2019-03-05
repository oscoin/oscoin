module Oscoin.API.Client
    ( MonadClient(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Types
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Node.Tree as STree
import qualified Radicle as Rad

class Monad m => MonadClient c m where
    submitTransaction :: RadTx c -> m (Result (TxSubmitResponse c (RadTx c)))

    -- | Returns an error result if a transaction with the given hash
    -- was not found.
    getTransaction :: Hashed c (RadTx c) -> m (Result (TxLookupResponse c))

    -- | Returns an error result if a value with the given key was not
    -- found.
    getState :: Proxy c -> STree.Path -> m (Result Rad.Value)
