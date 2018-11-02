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
import qualified Oscoin.Crypto.Hash as Crypto

import           Control.Concurrent.STM.TVar
import qualified Data.Map as Map

type StateStore st = Map StateHash st

newtype Handle st = Handle (TVar (StateStore st))

new :: MonadIO m => m (Handle st)
new = Handle <$> liftIO (newTVarIO mempty)

fromState :: Crypto.Hashable st => st -> StateStore st
fromState st = storeState st mempty

fromStateM :: (Crypto.Hashable st, MonadIO m) => st -> m (Handle st)
fromStateM st =
    Handle <$> liftIO (newTVarIO (storeState st mempty))

withHandle :: MonadIO m => Handle st -> (StateStore st -> m a) -> m a
withHandle (Handle tvar) f =
    f =<< liftIO (readTVarIO tvar)

lookupState :: StateHash -> StateStore st -> Maybe st
lookupState = Map.lookup

storeState :: Crypto.Hashable st => st -> StateStore st -> StateStore st
storeState s = Map.insert (Crypto.fromHashed $ Crypto.hash s) s

storeStateIO :: Crypto.Hashable st => Handle st -> st -> IO ()
storeStateIO (Handle tvar) =
    atomically . modifyTVar tvar . storeState
