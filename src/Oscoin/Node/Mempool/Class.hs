{-# LANGUAGE UndecidableInstances #-}

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

instance {-# OVERLAPPABLE #-}
    ( MonadTrans   t
    , Monad        (t m)
    , MonadMempool tx m
    ) => MonadMempool tx (t m)
  where
    addTxs    = lift . addTxs
    getTxs    = lift getTxs
    delTxs    = lift . delTxs
    numTxs    = lift numTxs
    lookupTx  = lift . lookupTx
    subscribe = lift subscribe
    {-# INLINE addTxs    #-}
    {-# INLINE getTxs    #-}
    {-# INLINE delTxs    #-}
    {-# INLINE numTxs    #-}
    {-# INLINE lookupTx  #-}
    {-# INLINE subscribe #-}