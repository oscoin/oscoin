module Oscoin.Storage.State
    ( MonadStateStore(..)
    , StateStore
    , lookupState
    , storeState
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (StateHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.HashStore

type StateStore c st m = HashStore c st m

class Monad m => MonadStateStore c st m | m -> st, m -> c where
    getStateStore :: m (HashStore c st m)

    default getStateStore
        :: (MonadStateStore c st m', MonadTrans t, m ~ t m')
        => m (HashStore c st m)
    getStateStore = hoistHashStore lift <$> lift getStateStore


lookupState :: forall c st m. (MonadStateStore c st m) => StateHash c -> m (Maybe st)
lookupState hash = do
    cs <- getStateStore
    lookupHashContent cs (Crypto.toHashed @c hash)


storeState :: forall c st m. (MonadStateStore c st m) => st -> m ()
storeState st = do
    cs <- getStateStore
    storeHashContent cs st
