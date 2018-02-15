module Oscoin.Consensus where

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.Node.State.Mempool as Mempool

run :: Environment -> Mempool.Handle tx -> m ()
run _env = undefined
