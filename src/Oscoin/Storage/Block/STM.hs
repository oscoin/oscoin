-- | In-memory block storage backed by STM.
module Oscoin.Storage.Block.STM
    ( withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain)
import           Oscoin.Crypto.Blockchain.Block (Block, Score, Sealed)
import           Oscoin.Crypto.Hash (Hash, Hashable)
import qualified Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Block.Pure as Pure

import           Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar)

-- | A bracket-style initialiser for an in-memory block store.
-- FIXME(adn): At the moment we are not consistent in how we compare chains
-- (or blocks): the /pure/ implementation uses a `ScoringFunction` whereas
-- things like the SQLite backend uses a `Block -> Score` function. We should
-- unify the two.
withBlockStore :: (Ord (Hash c), Hashable c tx)
               => Blockchain c tx s
               -- ^ The initial blockchain to initialise this store with
               -> (Block c tx (Sealed c s) -> Score)
               -- ^ A block scoring function
               -> (Abstract.BlockStore c tx s IO -> IO b)
               -- ^ Action to use the 'BlockStore'.
               -> IO b
withBlockStore chain score action = do
    hdl <- newTVarIO (Pure.initWithChain' chain score)
    let modifyHandle = atomically . modifyTVar' hdl
        newBlockStore  =
                Abstract.BlockStore {
                  Abstract.scoreBlock      = score
                , Abstract.insertBlock     = modifyHandle . Pure.insert
                , Abstract.getGenesisBlock = withHandle Pure.getGenesisBlock hdl
                , Abstract.lookupBlock     = \h -> withHandle (Pure.lookupBlock h) hdl
                , Abstract.lookupTx        = \h -> withHandle (Pure.lookupTx h) hdl
                , Abstract.getOrphans      = withHandle Pure.orphans hdl
                , Abstract.getBlocks       = \depth -> withHandle (Pure.getBlocks depth) hdl
                , Abstract.getTip          = withHandle Pure.getTip hdl
                }
    action newBlockStore
  where
      withHandle :: (Pure.Handle c tx s -> a) -> TVar (Pure.Handle c tx s) -> IO a
      withHandle f h = atomically (f <$> readTVar h)
