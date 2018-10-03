module Oscoin.Clock
    ( Tick
    , Timestamp(..)
    , Duration
    , duration
    , now
    , as
    , nanoseconds
    , microseconds
    , milliseconds
    , seconds
    , minutes
    , hours
    , MonadClock (..)
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Formatting as Fmt
import           System.Clock (Clock(Realtime), getTime, toNanoSecs)

-- | A Duration represents the elapsed time between two instants
-- as an Int64 nanosecond count. The representation limits the
-- largest representable duration to approximately 290 years.
type Duration = Int64

-- | Unix nanoseconds since epoch.
newtype Timestamp = Timestamp Duration deriving
    (Show, Read, Eq, Ord, Enum, Num, Real, Integral,
        Bounded, Generic, ToJSON, FromJSON, Serialise, Fmt.Buildable)

-- | Extracts the 'Duration' of a 'Timestamp'
duration :: Timestamp -> Duration
duration (Timestamp d) = d

-- | Returns the current Timestamp
now :: MonadIO m => m Timestamp
now = liftIO $ Timestamp . fromIntegral . toNanoSecs <$> getTime Realtime

-- | Common durations. There is no definition for units of Day or larger
-- to avoid confusion across daylight savings time zone transitions.
nanoseconds, microseconds, milliseconds, seconds, minutes, hours :: Duration
nanoseconds  = 1
microseconds = 1000 * nanoseconds
milliseconds = 1000 * microseconds
seconds      = 1000 * milliseconds
minutes      = 60   * seconds
hours        = 60   * minutes

-- | converts a 'Duration' to another unit specified by 'Duration'
as :: Duration -> Duration -> Double
as from to = to' + nsec / fromIntegral to where
    to'  = fromIntegral $ from `div` to
    nsec = fromIntegral $ from `mod` to

type Tick = Timestamp

class Monad m => MonadClock m where
    currentTick :: m Tick

    default currentTick
        :: (MonadTrans t, m ~ t m', MonadClock m')
        => m Tick
    currentTick = lift currentTick
    {-# INLINE currentTick #-}


instance MonadClock IO where
    currentTick = now
    {-# INLINE currentTick #-}
