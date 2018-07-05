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

instance MonadClock IO where
    currentTick = getPOSIXTime
    {-# INLINE currentTick #-}
