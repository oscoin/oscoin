module Oscoin.P2P where

import           Oscoin.Environment
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import           Oscoin.Prelude

run :: Environment -> Mempool.Handle tx -> STree.Handle -> IO ()
run _ _ _ = pass
