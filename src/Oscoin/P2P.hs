module Oscoin.P2P where

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.Node.State.Mempool as Mempool
import qualified Oscoin.Node.State.Tree as STree

run :: Environment -> Mempool.Handle tx -> STree.Handle -> IO ()
run _ _ _ = pass
