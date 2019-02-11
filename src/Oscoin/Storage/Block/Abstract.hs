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
data BlockStore tx s m = BlockStore
    { scoreBlock      :: Block tx s -> Score
    -- ^ When given a 'Block', return its 'Score'.
    , validateBlock   :: Validate tx s
    -- ^ Validity function for a 'Block'.
    , insertBlock     :: Block tx s -> m ()
    -- ^ Inserts a 'Block' into the store.
    , getGenesisBlock :: m (Block tx s)
    -- ^ Get the genesis block.
    , lookupBlock     :: BlockHash -> m (Maybe (Block tx s))
    -- ^ Lookups a 'Block' from the store when given its hash.
    , lookupTx        :: Hashed tx -> m (Maybe (TxLookup tx))
    -- ^ Lookups a transaction by its hash.
    , getOrphans      :: m (Set BlockHash)
    -- ^ The 'Hashed BlockHeader's of 'Block's for which we do not have a parent.
    , getBlocks       :: Depth -> m [Block tx s]
    -- ^ Returns the last N blocks, given a depth.
    , getTip          :: m (Block tx s)
    -- ^ Returns the tip of the chain.
    }

{------------------------------------------------------------------------------
Extra operations on the BlockStore, which are implementation-independent.
------------------------------------------------------------------------------}

-- | Given a natural transformation from @n@ to @m@, hoists a 'BlockStore'
-- initialised with a monad @n@ to work in the monad @m@.
hoistBlockStore
    :: forall tx s n m. (forall a. n a -> m a)
    -> BlockStore tx s n
    -> BlockStore tx s m
hoistBlockStore natTrans bs = BlockStore
    { scoreBlock      = scoreBlock bs
    , validateBlock   = validateBlock bs
    , insertBlock     = natTrans . insertBlock bs
    , getGenesisBlock = natTrans (getGenesisBlock bs)
    , lookupBlock     = natTrans . lookupBlock bs
    , lookupTx        = natTrans . lookupTx bs
    , getOrphans      = natTrans (getOrphans bs)
    , getBlocks       = natTrans . getBlocks bs
    , getTip          = natTrans (getTip bs)
    }

noValidation :: Validate tx s
noValidation _ _ = Right ()

defaultScoreFunction :: Block tx s -> Score
defaultScoreFunction = fst . decodeDifficulty . blockTargetDifficulty . blockHeader

-- | /O(n)/. A naive function to store blocks in linear time.
-- Useful for testing but discouraged for any serious production use.
-- NOTE(adn): It might be conceivable to have this function be upgraded as
-- a proper abstract function of the 'BlockStore', so that different implementations
-- can decide how to implement it efficiently.
insertBlocksNaive :: Monad m => BlockStore tx s m -> [Block tx s] -> m ()
insertBlocksNaive bs = traverse_ (insertBlock bs) . reverse

isNovelBlock :: Functor m => BlockStore tx s m -> BlockHash -> m Bool
isNovelBlock bs = map isNothing . lookupBlock bs
