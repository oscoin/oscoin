{- | Storing blocks
    This module provides an opaque interface on a 'BlockStore', a handle-like,
    stateful record which can be used to persist blocks on disk. This is the
    module where the abstract interface lives, but we also provides two
    concrete implementations: a pure one living in the 'InMemory' module and
    a SQLite-baked one living at 'SQLite'.
-}

module Oscoin.Storage.Block.Abstract
    ( BlockStoreReader(..)
    , BlockStore
    , hoistBlockStore

    , hoistBlockStoreReader
    , insertBlocksNaive
    , isNovelBlock

    -- * Internals
    , BlockStoreWriter(..)
    , hoistBlockStoreWriter
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (TxLookup)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Time.Chrono (NewestFirst, OldestFirst)

-- The full 'BlockStore' API is the composition of its public and private parts.
-- Morally, the 'BlockStore' offers an opaque interface to some kind of
-- persistent storage for blocks, with a set of operations to insert,
-- remove and retrieve them.
type BlockStore c tx s m =
    ( BlockStoreReader c tx s m
    , BlockStoreWriter c tx s m
    )

-- | The internals for this 'BlockStore', i.e. the private API which shouldn't
-- be visible to \"read-only consumers\".
data BlockStoreWriter c tx s m = BlockStoreWriter
    { insertBlock     :: Block c tx (Sealed c s) -> m ()
    -- ^ Inserts a 'Block' into the store. Generally speaking, application code
    -- is not expected to call this function directly, as it assumes that the
    -- parent of this input 'Block' is the same returned by 'getTip'.
    , switchToFork    :: Depth -> OldestFirst NonEmpty (Block c tx (Sealed c s)) -> m ()
    -- ^ @switchToFork n bs@ removes the @n@ latest blocks and calls
    -- 'insertBlock' for each block in @bs@ starting from the first
    -- (i.e. the oldest) in the list.
    }

-- | An handle over a block storage backend, namely to its public API.
data BlockStoreReader c tx s m = BlockStoreReader
    { getGenesisBlock :: m (Block c tx (Sealed c s))
    -- ^ Get the genesis block.
    , lookupBlock     :: BlockHash c -> m (Maybe (Block c tx (Sealed c s)))
    -- ^ Lookups a 'Block' from the store when given its hash.
    , lookupBlockByHeight :: Height -> m (Maybe (Block c tx (Sealed c s)))
    -- ^ Lookup a block, given its 'Height'.
    , lookupBlocksByHeight :: (Height,Height) -> m (OldestFirst [] (Block c tx (Sealed c s)))
    -- ^ Returns the blocks (if any) between the height range.
    , lookupTx        :: Hashed c tx -> m (Maybe (TxLookup c tx))
    -- ^ Lookups a transaction by its hash.
    , getBlocksByDepth :: Depth -> m (NewestFirst [] (Block c tx (Sealed c s)))
    -- ^ Returns the last N blocks, given the 'Depth'.
    , getBlocksByParentHash :: BlockHash c -> m (NewestFirst [] (Block c tx (Sealed c s)))
    -- ^ Returns the blocks starting from the input parent hash.
    , getTip          :: m (Block c tx (Sealed c s))
    -- ^ Returns the tip of the chain.
    }

{------------------------------------------------------------------------------
Extra operations on the BlockStore, which are implementation-independent.
------------------------------------------------------------------------------}

-- | Given a natural transformation from @n@ to @m@, hoists a 'BlockStore'
-- initialised with a monad @n@ to work in the monad @m@.
hoistBlockStoreReader
    :: forall c tx s n m. (forall a. n a -> m a)
    -> BlockStoreReader c tx s n
    -> BlockStoreReader c tx s m
hoistBlockStoreReader natTrans bs = BlockStoreReader
    { getGenesisBlock       = natTrans (getGenesisBlock bs)
    , lookupBlock           = natTrans . lookupBlock bs
    , lookupBlockByHeight   = natTrans . lookupBlockByHeight bs
    , lookupBlocksByHeight  = natTrans . lookupBlocksByHeight bs
    , lookupTx              = natTrans . lookupTx bs
    , getBlocksByDepth      = natTrans . getBlocksByDepth bs
    , getBlocksByParentHash = natTrans . getBlocksByParentHash bs
    , getTip                = natTrans (getTip bs)
    }

hoistBlockStoreWriter
    :: forall c tx s n m. (forall a. n a -> m a)
    -> BlockStoreWriter c tx s n
    -> BlockStoreWriter c tx s m
hoistBlockStoreWriter natTrans bs = BlockStoreWriter
    { insertBlock  = natTrans . insertBlock bs
    , switchToFork = \d -> natTrans . switchToFork bs d
    }

hoistBlockStore
    :: forall c tx s n m. (forall a. n a -> m a)
    -> BlockStore c tx s n
    -> BlockStore c tx s m
hoistBlockStore natTrans (public, private) =
  (hoistBlockStoreReader natTrans public, hoistBlockStoreWriter natTrans private)


-- | /O(n)/. A naive function to store blocks in linear time.
-- Useful for testing but discouraged for any serious production use.
-- NOTE(adn): It might be conceivable to have this function be upgraded as
-- a proper abstract function of the 'BlockStore', so that different implementations
-- can decide how to implement it efficiently.
insertBlocksNaive
    :: Monad m
    => BlockStoreWriter c tx s m
    -> OldestFirst [] (Block c tx (Sealed c s))
    -> m ()
insertBlocksNaive bs = traverse_ (insertBlock bs)

isNovelBlock :: Functor m => BlockStoreReader c tx s m -> BlockHash c -> m Bool
isNovelBlock bs = map isNothing . lookupBlock bs
