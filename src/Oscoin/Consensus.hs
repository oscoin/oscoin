module Oscoin.Consensus where

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.Node.State.Mempool as Mempool
import qualified Oscoin.Node.State.Tree as STree

run :: Environment -> Mempool.Handle tx -> STree.Handle -> m ()
run _env _ = undefined
