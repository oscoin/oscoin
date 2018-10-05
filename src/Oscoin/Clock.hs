module Oscoin.Clock
    ( MonadClock (..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Time (Timestamp, now)

class Monad m => MonadClock m where
    currentTick :: m Timestamp

    default currentTick
        :: (MonadTrans t, m ~ t m', MonadClock m')
        => m Timestamp
    currentTick = lift currentTick
    {-# INLINE currentTick #-}


instance MonadClock IO where
    currentTick = now
    {-# INLINE currentTick #-}
