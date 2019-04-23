-- | An opaque 'BlockTree', composed by its public and private API. Its main
-- responsibilities are:
--
-- 1. Storing main blocks.
-- 2. Storing orphan blocks.
-- 3. Performing chain selection.
-- 4. Perform full-chain validation.

module Oscoin.Storage.BlockTree.Internal where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (TxLookup)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Telemetry.Events (NotableEvent)
import           Oscoin.Time.Chrono (NewestFirst, OldestFirst)

type BlockTree c tx s m = (BlockTreeReader c tx s m, BlockTreeWriter c tx s m)

data BlockTreeReader c tx s m = BlockTreeReader
    { getGenesisBlock :: m (Block c tx (Sealed c s))
    -- ^ Get the genesis block.
    , lookupBlock     :: BlockHash c -> m (Maybe (Block c tx (Sealed c s)))
    -- ^ Lookups a 'Block' in the tree when given its hash.
    , lookupTx        :: Hashed c tx -> m (Maybe (TxLookup c tx))
    -- ^ Lookups a transaction by its hash.
    , getBlocksByDepth :: Depth -> m (NewestFirst [] (Block c tx (Sealed c s)))
    -- ^ Returns the last N blocks, given the 'Depth'.
    , getBlocksByParentHash :: BlockHash c -> m (NewestFirst [] (Block c tx (Sealed c s)))
    -- ^ Returns the blocks starting from the input parent hash.
    , getTip          :: m (Block c tx (Sealed c s))
    -- ^ Returns the tip of the best chain in the tree.
    }

data BlockTreeWriter c tx s m = BlockTreeWriter
    { insertBlock     :: Block c tx (Sealed c s) -> m (OldestFirst [] NotableEvent)
    -- ^ Inserts a 'Block' into the tree. The 'BlockTree' implementation is
    -- responsible for either storing this block on the tip (if it extends the
    -- main chain) or as an orphan.
    }
