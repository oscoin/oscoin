{-# LANGUAGE DefaultSignatures #-}

module Oscoin.Consensus.BlockStore.Class where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain, Lineage)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash, Orphan)
import           Oscoin.Crypto.Hash (Hashed)

class (Monad m) => MonadBlockStore tx s m | m -> tx, m -> s where
    -- | Store a block in the block-store.
    storeBlock :: Block tx (Orphan s) -> m ()

    -- | Lookup a 'Block' by its header.
    lookupBlock :: BlockHash -> m (Maybe (Block tx s))

    -- | Get the genesis block.
    getGenesisBlock :: m (Block tx s)

    -- | Lookup a transaction by its hash.
    lookupTx :: Hashed tx -> m (Maybe (tx, Lineage))

    -- | The 'Hashed BlockHeader's of 'Block's for which we do not have a parent.
    orphans :: m (Set BlockHash)

    -- | Returns the maximum chain, according to the ordering function provided.
    maximumChainBy
        :: (Blockchain tx s -> Blockchain tx s -> Ordering)
        -> m (Blockchain tx s)


    default storeBlock
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => Block tx (Orphan s) -> m ()
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
        => Hashed tx -> m (Maybe (tx, Lineage))
    lookupTx = lift . lookupTx
    {-# INLINE lookupTx #-}

    default orphans
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => m (Set BlockHash)
    orphans = lift orphans
    {-# INLINE orphans #-}

    default maximumChainBy
        :: (MonadBlockStore tx s m', MonadTrans t, m ~ t m')
        => (Blockchain tx s -> Blockchain tx s -> Ordering)
        -> m (Blockchain tx s)
    maximumChainBy = lift . maximumChainBy
    {-# INLINE maximumChainBy #-}
