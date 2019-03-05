-- | Implements general state storage for blockchains.
module Oscoin.Storage.State
    ( lookupState
    , storeState
    , storeStateIO
    , withHandle
    , StateStore
    , Handle
    , new
    , fromState
    , fromStateM
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (StateHash)
import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Crypto.Hash as Crypto

import           Control.Concurrent.STM.TVar
import qualified Data.Map as Map

type StateStore c st = Map (StateHash c) st

newtype Handle c st = Handle (TVar (StateStore c st))

new :: (Ord (Hash c), MonadIO m) => m (Handle c st)
new = Handle <$> liftIO (newTVarIO mempty)

fromState :: (Ord (Hash c), Crypto.Hashable c st) => st -> StateStore c st
fromState st = storeState st mempty

fromStateM
    :: ( Ord (Hash c)
       , Crypto.Hashable c st
       , MonadIO m
       )
    => st
    -> m (Handle c st)
fromStateM st =
    Handle <$> liftIO (newTVarIO (storeState st mempty))

withHandle :: MonadIO m => Handle c st -> (StateStore c st -> m a) -> m a
withHandle (Handle tvar) f =
    f =<< liftIO (readTVarIO tvar)

lookupState :: Ord (StateHash c) => StateHash c -> StateStore c st -> Maybe st
lookupState = Map.lookup

storeState
    :: ( Ord (Hash c)
       , Crypto.Hashable c st
       )
    => st
    -> StateStore c st
    -> StateStore c st
storeState s = Map.insert (Crypto.fromHashed $ Crypto.hash s) s

storeStateIO
    :: ( Ord (Hash c)
       , Crypto.Hashable c st
       )
    => Handle c st
    -> st
    -> IO ()
storeStateIO (Handle tvar) =
    atomically . modifyTVar tvar . storeState
