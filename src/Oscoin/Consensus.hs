module Oscoin.Consensus where

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.Node.State.Mempool as Mempool
import           Oscoin.Node.State.Mempool (MonadMempool)
import qualified Oscoin.Node.State.Tree as STree
import           Oscoin.Node.Channel
import           Oscoin.Crypto.PubKey (Signed, unsign)

import qualified Oscoin.Org.Transaction as Org

-- TODO: We don't want the MonadIO constraint here. This is only due to
-- using an IORef instead of an STM var in STree.

run
    :: (MonadIO m, MonadMempool (Signed Org.Tx) r m, MonadSTM m)
    => Environment
    -> STree.Handle
    -> m ()
run _ st = do
    sub <- Mempool.subscribe "consensus"
    loop st sub

loop :: (MonadIO m, MonadSTM m) => STree.Handle -> Channel (Signed Org.Tx) -> m a
loop st sub = do
    evs <- Mempool.flushChannel sub
    --
    -- TODO: Obviously consensus shouldn't just apply the mempool transactions
    -- directly to the state tree.
    --
    for_ evs $ \(Event tx) -> do
        STree.update st (Org.applyTransaction (unsign tx))
    loop st sub
