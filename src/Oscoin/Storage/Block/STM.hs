-- | In-memory block storage backed by STM.
module Oscoin.Storage.Block.STM
    ( Handle
    , new
    , newIO
    , put
    , for
    ) where

import           Oscoin.Prelude hiding (for, put)

import           Oscoin.Crypto.Blockchain.Block (Block, Orphan)
import           Oscoin.Storage.Block

import           Control.Concurrent.STM
                 (TVar, modifyTVar', newTVar, newTVarIO, readTVar)

-- | A handle to an in-memory block store.
newtype Handle tx s = Handle (TVar (BlockStore tx s))

new :: BlockStore tx s -> STM (Handle tx s)
new bs = Handle <$> newTVar bs

newIO :: BlockStore tx s -> IO (Handle tx s)
newIO bs = Handle <$> newTVarIO bs

put :: (Ord tx) => Handle tx s -> Block tx (Orphan s) -> STM ()
put (Handle tvar) = modifyTVar' tvar . insert

for :: Handle tx s -> (BlockStore tx s -> a) -> STM a
for (Handle tvar) f = f <$> readTVar tvar
