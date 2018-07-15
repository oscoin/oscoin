{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Consensus.BlockStore.Class where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHeader)
import           Oscoin.Crypto.Hash (Hashed)

class Monad m => MonadBlockStore tx m | m -> tx where
    -- | Store a block in the block-store.
    storeBlock :: Block tx -> m ()

    -- | Lookup a 'Block' by its header.
    lookupBlock :: Hashed BlockHeader -> m (Maybe (Block tx))

    -- | Lookup a transaction by its hash.
    lookupTx :: Hashed tx -> m (Maybe tx)

    -- | The 'Hashed BlockHeader's of 'Block's for which we do not have a parent.
    orphans :: m (Set (Hashed BlockHeader))

    -- | Returns the maximum chain, according to the ordering function provided.
    maximumChainBy
        :: (Blockchain tx -> Blockchain tx -> Ordering)
        -> m (Blockchain tx)

instance {-# OVERLAPPABLE #-}
    ( MonadTrans t
    , Monad (t m)
    , MonadBlockStore tx m
    ) => MonadBlockStore tx (t m)
  where
    storeBlock     = lift . storeBlock
    lookupBlock    = lift . lookupBlock
    lookupTx       = lift . lookupTx
    orphans        = lift orphans
    maximumChainBy = lift . maximumChainBy
    {-# INLINE storeBlock     #-}
    {-# INLINE lookupBlock    #-}
    {-# INLINE lookupTx       #-}
    {-# INLINE orphans        #-}
    {-# INLINE maximumChainBy #-}
