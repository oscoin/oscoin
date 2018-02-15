module Oscoin.P2P where

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.Node.State.Mempool as Mempool

run :: Environment -> Mempool.Handle tx -> IO ()
run _ _ = pass
