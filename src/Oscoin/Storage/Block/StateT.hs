-- | In-memory block storage backed by StateT over Identity.
module Oscoin.Storage.Block.StateT
    ( withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
                 (Block, BlockHash, Score, Sealed)
import           Oscoin.Crypto.Hash (Hashable)
import qualified Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Block.Pure as Pure

type Handle c tx s m = StateT (Pure.Handle c tx s) m

-- | A bracket-style initialiser for an in-memory block store.
withBlockStore :: (Ord (BlockHash c), Monad m, Hashable c tx)
               => Block c tx (Sealed c s)
               -- ^ The genesis block (used to initialise the store)
               -> (Block c tx (Sealed c s) -> Score)
               -- ^ A block scoring function
               -> (Abstract.BlockStore c tx s (Handle c tx s m) -> Handle c tx s m b)
               -- ^ Action to use the 'BlockStore'.
               -> m b
withBlockStore gen score action =
    let hdl = Pure.genesisBlockStore gen
        newBlockStore  =
                Abstract.BlockStore {
                  Abstract.scoreBlock      = score
                , Abstract.insertBlock     = modify . Pure.insert
                , Abstract.getGenesisBlock = gets Pure.getGenesisBlock
                , Abstract.lookupBlock     = gets . Pure.lookupBlock
                , Abstract.lookupTx        = gets . Pure.lookupTx
                , Abstract.getOrphans      = gets Pure.orphans
                , Abstract.getBlocks       = gets . Pure.getBlocks
                , Abstract.getTip          = gets Pure.getTip
                }
    in evalStateT (action newBlockStore) hdl
