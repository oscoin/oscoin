-- | In-memory block storage backed by STM.
module Oscoin.Storage.Block.STM
    ( withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain)
import           Oscoin.Crypto.Blockchain.Block (ScoreFn, Sealed)
import           Oscoin.Crypto.Hash (Hash, Hashable)
import qualified Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Block.Pure as Pure
import           Oscoin.Time.Chrono as Chrono

import           Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar)

-- | A bracket-style initialiser for an in-memory block store.
withBlockStore
    :: (Ord (Hash c), Hashable c tx)
    => Blockchain c tx s
    -- ^ The initial blockchain to initialise this store with
    -> ScoreFn c tx (Sealed c s)
    -- ^ A block scoring function
    -> (Abstract.BlockStore c tx s IO -> IO b)
    -- ^ Action to use the 'BlockStore'.
    -> IO b
withBlockStore chain score action = do
    hdl <- newTVarIO (Pure.initWithChain chain score)
    let modifyHandle = atomically . modifyTVar' hdl
        newBlockStore  =
            ( Abstract.BlockStoreReader
                { Abstract.getGenesisBlock  = withHandle Pure.getGenesisBlock hdl
                , Abstract.lookupBlock      = \h -> withHandle (Pure.lookupBlock h) hdl
                , Abstract.lookupTx         = \h -> withHandle (Pure.lookupTx h) hdl
                , Abstract.getBlocksByDepth = \depth ->
                      withHandle (Chrono.NewestFirst <$> Pure.getBlocks depth) hdl
                , Abstract.getBlocksByParentHash = \h ->
                      withHandle (Chrono.NewestFirst <$> Pure.getChainSuffix h) hdl
                , Abstract.getTip          = withHandle Pure.getTip hdl
                }
            , Abstract.BlockStoreWriter
                { Abstract.insertBlock     = modifyHandle . Pure.insert
                -- The STM blockstore piggybacks on the pure store which doesn't
                -- really support switchToFork.
                , Abstract.switchToFork    = \_ _ -> pure ()
                }
            )
    action newBlockStore
  where
      withHandle :: (Pure.Handle c tx s -> a) -> TVar (Pure.Handle c tx s) -> IO a
      withHandle f h = atomically (f <$> readTVar h)
