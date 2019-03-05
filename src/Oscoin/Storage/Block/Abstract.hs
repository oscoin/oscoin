{- | Storing blocks
    This module provides an opaque interface on a 'BlockStore', a handle-like,
    stateful record which can be used to persist blocks on disk. This is the
    module where the abstract interface lives, but we also provides two
    concrete implementations: a pure one living in the 'InMemory' module and
    a SQLite-baked one living at 'SQLite'.
-}

module Oscoin.Storage.Block.Abstract
    ( BlockStore(..)

    , hoistBlockStore
    , noValidation
    , defaultScoreFunction
    , insertBlocksNaive
    , isNovelBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types (Validate)
import           Oscoin.Crypto.Blockchain (TxLookup)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashed)

-- | An handle over a block storage backend.
data BlockStore c tx s m = BlockStore
    { scoreBlock      :: Block c tx (Sealed c s) -> Score
    -- ^ When given a 'Block', return its 'Score'.
    , insertBlock     :: Block c tx (Sealed c s) -> m ()
    -- ^ Inserts a 'Block' into the store.
    , getGenesisBlock :: m (Block c tx (Sealed c s))
    -- ^ Get the genesis block.
    , lookupBlock     :: BlockHash c -> m (Maybe (Block c tx (Sealed c s)))
    -- ^ Lookups a 'Block' from the store when given its hash.
    , lookupTx        :: Hashed c tx -> m (Maybe (TxLookup c tx))
    -- ^ Lookups a transaction by its hash.
    , getOrphans      :: m (Set (BlockHash c))
    -- ^ The 'Hashed BlockHeader's of 'Block's for which we do not have a parent.
    , getBlocks       :: Depth -> m [Block c tx (Sealed c s)]
    -- ^ Returns the last N blocks, given a depth.
    , getTip          :: m (Block c tx (Sealed c s))
    -- ^ Returns the tip of the chain.
    }

{------------------------------------------------------------------------------
Extra operations on the BlockStore, which are implementation-independent.
------------------------------------------------------------------------------}

-- | Given a natural transformation from @n@ to @m@, hoists a 'BlockStore'
-- initialised with a monad @n@ to work in the monad @m@.
hoistBlockStore
    :: forall c tx s n m. (forall a. n a -> m a)
    -> BlockStore c tx s n
    -> BlockStore c tx s m
hoistBlockStore natTrans bs = BlockStore
    { scoreBlock      = scoreBlock bs
    , insertBlock     = natTrans . insertBlock bs
    , getGenesisBlock = natTrans (getGenesisBlock bs)
    , lookupBlock     = natTrans . lookupBlock bs
    , lookupTx        = natTrans . lookupTx bs
    , getOrphans      = natTrans (getOrphans bs)
    , getBlocks       = natTrans . getBlocks bs
    , getTip          = natTrans (getTip bs)
    }

noValidation :: Validate c tx s
noValidation _ _ = Right ()

defaultScoreFunction :: Block c tx s -> Score
defaultScoreFunction = fst . decodeDifficulty . blockTargetDifficulty . blockHeader

-- | /O(n)/. A naive function to store blocks in linear time.
-- Useful for testing but discouraged for any serious production use.
-- NOTE(adn): It might be conceivable to have this function be upgraded as
-- a proper abstract function of the 'BlockStore', so that different implementations
-- can decide how to implement it efficiently.
insertBlocksNaive
    :: Monad m
    => BlockStore c tx s m
    -> [Block c tx (Sealed c s)]
    -> m ()
insertBlocksNaive bs = traverse_ (insertBlock bs) . reverse

isNovelBlock :: Functor m => BlockStore c tx s m -> BlockHash c -> m Bool
isNovelBlock bs = map isNothing . lookupBlock bs
