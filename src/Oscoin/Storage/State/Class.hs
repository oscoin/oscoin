module Oscoin.Storage.State.Class
    ( MonadStateStore(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (StateHash)

class Monad m => MonadStateStore c st m | m -> st, m -> c where
    -- | Lookup a state by hash.
    lookupState :: StateHash c -> m (Maybe st)

    -- | Store a state.
    storeState  :: st -> m ()

    default lookupState
        :: (MonadStateStore c st m', MonadTrans t, m ~ t m')
        => StateHash c -> m (Maybe st)
    lookupState = lift . lookupState

    default storeState
        :: (MonadStateStore c st m', MonadTrans t, m ~ t m')
        => st -> m ()
    storeState = lift . storeState
