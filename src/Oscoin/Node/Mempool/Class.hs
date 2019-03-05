module Oscoin.Node.Mempool.Class
    ( MonadMempool (..)

    , Event (..)
    , Channel
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Node.Mempool.Event (Channel, Event(..))

-- MonadMempool ---------------------------------------------------------------

class Monad m => MonadMempool c tx m | m -> tx, m -> c where
    -- | Add transactions to the mempool, notifying all subscribers.
    addTxs :: Foldable t => t tx -> m ()

    -- | Get all the transactions in the mempool.
    getTxs :: m [(Hashed c tx, tx)]

    -- | Remove the supplied transactions from the mempool.
    delTxs :: Foldable t => t tx -> m ()

    -- | Return the number of transactions in the mempool.
    numTxs :: m Int

    -- | Lookup a transaction by its hash.
    lookupTx :: Hashed c tx -> m (Maybe tx)

    -- | Subscribe to mempool events.
    subscribe :: m (Channel tx)

    default addTxs
        :: (MonadMempool c tx m', MonadTrans t, m ~ t m', Foldable f)
        => f tx -> m ()
    addTxs = lift . addTxs
    {-# INLINE addTxs #-}

    default getTxs
        :: (MonadMempool c tx m', MonadTrans t, m ~ t m')
        => m [(Hashed c tx, tx)]
    getTxs = lift getTxs
    {-# INLINE getTxs #-}

    default delTxs
        :: (MonadMempool c tx m', MonadTrans t, m ~ t m', Foldable f)
        => f tx -> m ()
    delTxs = lift . delTxs
    {-# INLINE delTxs #-}

    default numTxs
        :: (MonadMempool c tx m', MonadTrans t, m ~ t m')
        => m Int
    numTxs = lift numTxs
    {-# INLINE numTxs #-}

    default lookupTx
        :: (MonadMempool c tx m', MonadTrans t, m ~ t m')
        => Hashed c tx -> m (Maybe tx)
    lookupTx = lift . lookupTx
    {-# INLINE lookupTx #-}

    default subscribe
        :: (MonadMempool c tx m', MonadTrans t, m ~ t m')
        => m (Channel tx)
    subscribe = lift subscribe
    {-# INLINE subscribe #-}
