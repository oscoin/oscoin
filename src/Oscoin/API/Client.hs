module Oscoin.API.Client
    ( MonadClient(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Types
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Data.RadicleTx as RadicleTx
import qualified Oscoin.Node.Tree as STree

class Monad m => MonadClient c m where
    submitTransaction :: RadicleTx.RadTx c -> m (Result (TxSubmitResponse c (RadicleTx.RadTx c)))

    -- | Returns an error result if a transaction with the given hash
    -- was not found.
    getTransaction :: Hashed c (RadicleTx.RadTx c) -> m (Result (TxLookupResponse c))

    -- | Returns an error result if a value with the given key was not
    -- found.
    getState :: Proxy c -> STree.Path -> m (Result RadicleTx.Value)
