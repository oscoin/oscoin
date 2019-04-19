module Oscoin.Storage.BlockTree.Pure
    ( withBlockTree
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain)
import           Oscoin.Crypto.Blockchain.Block (ScoreFn, Sealed)
import           Oscoin.Crypto.Hash (Hash, Hashable)
import qualified Oscoin.Storage.Block.Pure as Pure
import qualified Oscoin.Storage.BlockTree.Internal as Abstract
import           Oscoin.Time.Chrono as Chrono

import           Control.Concurrent.STM (TVar, newTVarIO, readTVar, stateTVar)
import           Formatting

-- | A bracket-style initialiser for an in-memory block-tree.
withBlockTree
    :: ( Hashable c tx
       , Buildable (Hash c)
       )
    => Blockchain c tx s
    -- ^ The initial blockchain to initialise this store with
    -> ScoreFn c tx (Sealed c s)
    -- ^ A block scoring function
    -> (Abstract.BlockTree c tx s IO -> IO b)
    -- ^ Action to use the 'BlockTree'.
    -> IO b
withBlockTree chain score action = do
    hdl <- newTVarIO (Pure.initWithChain chain score)
    let modifyHandle = atomically . stateTVar hdl
        newBlockStore  =
            ( Abstract.BlockTreeReader
                { Abstract.getGenesisBlock  = withHandle Pure.getGenesisBlock hdl
                , Abstract.lookupBlock      = \h -> withHandle (Pure.lookupBlock h) hdl
                , Abstract.lookupTx         = \h -> withHandle (Pure.lookupTx h) hdl
                , Abstract.getBlocksByDepth = \depth ->
                      withHandle (Chrono.NewestFirst <$> Pure.getBlocks depth) hdl
                , Abstract.getBlocksByParentHash = \h ->
                      withHandle (Chrono.NewestFirst <$> Pure.getChainSuffix h) hdl
                , Abstract.getTip          = withHandle Pure.getTip hdl
                }
            , Abstract.BlockTreeWriter
                { Abstract.insertBlock     = \blk ->
                    modifyHandle (swap . Pure.insert blk)
                }
            )
    action newBlockStore
  where
      withHandle :: (Pure.Handle c tx s -> a) -> TVar (Pure.Handle c tx s) -> IO a
      withHandle f h = atomically (f <$> readTVar h)

