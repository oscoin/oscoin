module Oscoin.Storage.State.Class
    ( MonadStateStore(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (StateHash)

class Monad m => MonadStateStore st m | m -> st where
    -- | Lookup a state by hash.
    lookupState :: StateHash -> m (Maybe st)

    -- | Store a state.
    storeState  :: st -> m ()

    default lookupState
        :: (MonadStateStore st m', MonadTrans t, m ~ t m')
        => StateHash -> m (Maybe st)
    lookupState = lift . lookupState

    default storeState
        :: (MonadStateStore st m', MonadTrans t, m ~ t m')
        => st -> m ()
    storeState = lift . storeState
