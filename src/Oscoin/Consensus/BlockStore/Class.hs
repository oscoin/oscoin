{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Consensus.BlockStore.Class where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHeader, Orphan)
import           Oscoin.Crypto.Hash (Hashed)

class (Monad m) => MonadBlockStore tx s m | m -> tx, m -> s where
    -- | Store a block in the block-store.
    storeBlock :: Block tx (Orphan s) -> m ()

    -- | Lookup a 'Block' by it's header
    lookupBlock :: Hashed (BlockHeader ()) -> m (Maybe (Block tx s))

    -- | The 'Hashed BlockHeader's of 'Block's for which we do not have a parent
    orphans :: m (Set (Hashed (BlockHeader ())))

    -- | Returns the maximum chain, according to the ordering function provided.
    maximumChainBy
        :: (Blockchain tx s -> Blockchain tx s -> Ordering)
        -> m (Blockchain tx s)

instance {-# OVERLAPPABLE #-}
    ( MonadTrans t
    , Monad (t m)
    , MonadBlockStore s tx m
    ) => MonadBlockStore s tx (t m)
  where
    storeBlock     = lift . storeBlock
    lookupBlock    = lift . lookupBlock
    orphans        = lift orphans
    maximumChainBy = lift . maximumChainBy
    {-# INLINE storeBlock     #-}
    {-# INLINE lookupBlock    #-}
    {-# INLINE orphans        #-}
    {-# INLINE maximumChainBy #-}
