module Oscoin.Storage.Block
    ( Handle
    , new
    , put
    , for
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (BlockStore)
import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Crypto.Blockchain.Block (Block)

import           Control.Concurrent.STM (TVar, modifyTVar', newTVar, readTVar)

newtype Handle tx = Handle (TVar (BlockStore tx))

new :: (MonadSTM m, Functor m) => BlockStore tx -> m (Handle tx)
new bs = Handle <$> liftSTM (newTVar bs)

put :: (Ord tx, MonadSTM m) => Handle tx -> Block tx -> m ()
put (Handle tvar) =
    liftSTM . modifyTVar' tvar . BlockStore.insert

for :: (Monad m, MonadSTM m) => Handle tx -> (BlockStore tx -> a) -> m a
for (Handle tvar) f =
    f <$> liftSTM (readTVar tvar)
