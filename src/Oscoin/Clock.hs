module Oscoin.Clock
    ( Tick
    , MonadClock (..)
    ) where

import           Oscoin.Prelude

import           Data.Time.Clock (NominalDiffTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)

type Tick = NominalDiffTime

class Monad m => MonadClock m where
    currentTick :: m Tick

    default currentTick
        :: (MonadTrans t, m ~ t m', MonadClock m')
        => m Tick
    currentTick = lift currentTick
    {-# INLINE currentTick #-}


instance MonadClock IO where
    currentTick = getPOSIXTime
    {-# INLINE currentTick #-}
