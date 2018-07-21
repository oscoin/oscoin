module Oscoin.Storage.Block
    ( Handle
    , new
    , put
    , for
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (BlockStore)
import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Crypto.Blockchain.Block (Block, Orphan)

import           Control.Concurrent.STM (TVar, modifyTVar', newTVar, readTVar)

newtype Handle tx s = Handle (TVar (BlockStore tx s))

new :: (MonadSTM m, Functor m) => BlockStore tx s -> m (Handle tx s)
new bs = Handle <$> liftSTM (newTVar bs)

put :: (Ord tx, MonadSTM m) => Handle tx s -> Block tx (Orphan s) -> m ()
put (Handle tvar) =
    liftSTM . modifyTVar' tvar . BlockStore.insert

for :: (Monad m, MonadSTM m) => Handle tx s -> (BlockStore tx s -> a) -> m a
for (Handle tvar) f =
    f <$> liftSTM (readTVar tvar)
