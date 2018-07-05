module Oscoin.Consensus where

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Tree as STree
import           Oscoin.Crypto.PubKey (Signed, unsign)

import qualified Oscoin.Account.Transaction as Account

import qualified Control.Concurrent.STM as STM

-- TODO: We don't want the MonadIO constraint here. This is only due to
-- using an IORef instead of an STM var in STree.

run
    :: (MonadIO m, MonadMempool (Signed Account.Tx) m, MonadSTM m)
    => Environment
    -> STree.Handle
    -> m ()
run _ st = do
    chan <- Mempool.subscribe
    loop st chan

loop :: (MonadIO m, MonadSTM m) => STree.Handle -> Mempool.Channel (Signed Account.Tx) -> m a
loop st chan = do
    ev <- liftSTM $ STM.readTChan chan
    case ev of
        Mempool.Insert txs ->
            --
            -- TODO: Obviously consensus shouldn't just apply the mempool transactions
            -- directly to the state tree.
            --
            for_ txs $
                STree.update st . Account.applyTransaction . unsign
        _ -> pure ()

    loop st chan
