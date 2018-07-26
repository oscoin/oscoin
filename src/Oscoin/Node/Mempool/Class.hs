{-# LANGUAGE DefaultSignatures #-}

module Oscoin.Node.Mempool.Class
    ( MonadMempool (..)

    , Event (..)
    , Channel
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Node.Mempool.Event (Channel, Event(..))

-- MonadMempool ---------------------------------------------------------------

class Monad m => MonadMempool tx m | m -> tx where
    -- | Add transactions to the mempool, notifying all subscribers.
    addTxs :: Foldable t => t tx -> m ()

    -- | Get all the transactions in the mempool.
    getTxs :: m [(Hashed tx, tx)]

    -- | Remove the supplied transactions from the mempool.
    delTxs :: Foldable t => t tx -> m ()

    -- | Return the number of transactions in the mempool.
    numTxs :: m Int

    -- | Lookup a transaction by its hash.
    lookupTx :: Hashed tx -> m (Maybe tx)

    -- | Subscribe to mempool events.
    subscribe :: m (Channel tx)

    default addTxs
        :: (MonadMempool tx m', MonadTrans t, m ~ t m', Foldable f)
        => f tx -> m ()
    addTxs = lift . addTxs
    {-# INLINE addTxs #-}

    default getTxs
        :: (MonadMempool tx m', MonadTrans t, m ~ t m')
        => m [(Hashed tx, tx)]
    getTxs = lift getTxs
    {-# INLINE getTxs #-}

    default delTxs
        :: (MonadMempool tx m', MonadTrans t, m ~ t m', Foldable f)
        => f tx -> m ()
    delTxs = lift . delTxs
    {-# INLINE delTxs #-}

    default numTxs
        :: (MonadMempool tx m', MonadTrans t, m ~ t m')
        => m Int
    numTxs = lift numTxs
    {-# INLINE numTxs #-}

    default lookupTx
        :: (MonadMempool tx m', MonadTrans t, m ~ t m')
        => Hashed tx -> m (Maybe tx)
    lookupTx = lift . lookupTx
    {-# INLINE lookupTx #-}

    default subscribe
        :: (MonadMempool tx m', MonadTrans t, m ~ t m')
        => m (Channel tx)
    subscribe = lift subscribe
    {-# INLINE subscribe #-}
