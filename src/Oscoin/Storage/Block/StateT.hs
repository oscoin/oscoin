-- | In-memory block storage backed by StateT over Identity.
module Oscoin.Storage.Block.StateT
    ( withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Validate)
import           Oscoin.Crypto.Blockchain.Block (Block, Score)
import           Oscoin.Crypto.Hash (Hashable)
import qualified Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Block.Pure as Pure

type Handle tx s m = StateT (Pure.Handle tx s) m

-- | A bracket-style initialiser for an in-memory block store.
withBlockStore :: (Monad m, Hashable tx)
               => Block tx s
               -- ^ The genesis block (used to initialise the store)
               -> (Block tx s -> Score)
               -- ^ A block scoring function
               -> Validate tx s
               -- ^ A block validation function
               -> (Abstract.BlockStore tx s (Handle tx s m) -> Handle tx s m b)
               -- ^ Action to use the 'BlockStore'.
               -> m b
withBlockStore gen score validate action =
    let hdl = Pure.genesisBlockStore gen
        newBlockStore  =
                Abstract.BlockStore {
                  Abstract.scoreBlock      = score
                , Abstract.validateBlock   = validate
                , Abstract.insertBlock     = modify . Pure.insert
                , Abstract.getGenesisBlock = gets Pure.getGenesisBlock
                , Abstract.lookupBlock     = gets . Pure.lookupBlock
                , Abstract.lookupTx        = gets . Pure.lookupTx
                , Abstract.getOrphans      = gets Pure.orphans
                , Abstract.getBlocks       = gets . Pure.getBlocks
                , Abstract.getTip          = gets Pure.getTip
                }
    in evalStateT (action newBlockStore) hdl
