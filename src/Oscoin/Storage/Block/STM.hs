-- | In-memory block storage backed by STM.
module Oscoin.Storage.Block.STM
    ( withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Validate)
import           Oscoin.Crypto.Blockchain (Blockchain)
import           Oscoin.Crypto.Blockchain.Block (Block, Score)
import           Oscoin.Crypto.Hash (Hashable)
import qualified Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Block.Pure as Pure

import           Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar)

-- | A bracket-style initialiser for an in-memory block store.
-- FIXME(adn): At the moment we are not consistent in how we compare chains
-- (or blocks): the /pure/ implementation uses a `ScoringFunction` whereas
-- things like the SQLite backend uses a `Block -> Score` function. We should
-- unify the two.
withBlockStore :: Hashable tx
               => Blockchain tx s
               -- ^ The initial blockchain to initialise this store with
               -> (Block tx s -> Score)
               -- ^ A block scoring function
               -> Validate tx s
               -- ^ A block validation function
               -> (Abstract.BlockStore tx s IO -> IO b)
               -- ^ Action to use the 'BlockStore'.
               -> IO b
withBlockStore chain score validate action = do
    hdl <- newTVarIO (Pure.initWithChain' chain score)
    let modifyHandle = atomically . modifyTVar' hdl
        newBlockStore  =
                Abstract.BlockStore {
                  Abstract.scoreBlock      = score
                , Abstract.validateBlock   = validate
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
      withHandle :: (Pure.Handle tx s -> a) -> TVar (Pure.Handle tx s) -> IO a
      withHandle f h = atomically (f <$> readTVar h)
