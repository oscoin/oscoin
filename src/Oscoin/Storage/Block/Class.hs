module Oscoin.Storage.Block.Class where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (TxLookup)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash, Depth)
import           Oscoin.Crypto.Hash (Hashed)

class (Monad m) => MonadBlockStore tx s m | m -> tx, m -> s where
    -- | Store a block in the block-store.
    storeBlock :: Block tx s -> m ()

    -- | Lookup a 'Block' by its header.
    lookupBlock :: BlockHash -> m (Maybe (Block tx s))

    -- | Get the genesis block.
    getGenesisBlock :: m (Block tx s)

    -- | Lookup a transaction by its hash.
    lookupTx :: Hashed tx -> m (Maybe (TxLookup tx))

    -- | The 'Hashed BlockHeader's of 'Block's for which we do not have a parent.
    getOrphans :: m (Set BlockHash)

    -- | Returns the last N blocks, given a depth.
    getBlocks :: Depth -> m [Block tx s]

    -- | Returns the tip of the chain.
    getTip :: m (Block tx s)
    getTip = headDef (panic "Empty Blockchain!") <$> getBlocks 1


    default storeBlock
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => Block tx s -> m ()
    storeBlock = lift . storeBlock
    {-# INLINE storeBlock #-}

    default lookupBlock
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => BlockHash -> m (Maybe (Block tx s))
    lookupBlock = lift . lookupBlock
    {-# INLINE lookupBlock #-}

    default getGenesisBlock
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => m (Block tx s)
    getGenesisBlock = lift getGenesisBlock
    {-# INLINE getGenesisBlock #-}

    default lookupTx
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => Hashed tx -> m (Maybe (TxLookup tx))
    lookupTx = lift . lookupTx
    {-# INLINE lookupTx #-}

    default getOrphans
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => m (Set BlockHash)
    getOrphans = lift getOrphans
    {-# INLINE getOrphans #-}

    default getBlocks
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => Depth
        -> m [Block tx s]
    getBlocks = lift . getBlocks
    {-# INLINE getBlocks #-}
