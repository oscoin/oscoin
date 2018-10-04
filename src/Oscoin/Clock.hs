module Oscoin.Clock
    ( Tick
    , Timestamp(..)
    , Duration
    , Unit(..)
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

-- | A sum type of common Duration units. There is no definition for units
-- of Day or larger to avoid confusion across daylight savings time zone transitions.
data Unit = Nanoseconds
          | Microseconds
          | Milliseconds
          | Seconds
          | Minutes
          | Hours

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

-- | Converts a Unit into its respective Duration in nanoseconds
unit :: Unit -> Duration
unit Nanoseconds  = nanoseconds
unit Microseconds = microseconds
unit Milliseconds = milliseconds
unit Seconds      = seconds
unit Minutes      = minutes
unit Hours        = hours

-- | converts a 'Duration' to another 'Unit'
as :: Duration -> Unit -> Double
from `as` to = unit' + nsec / fromIntegral to' where
    unit' = fromIntegral $ from `div` to'
    nsec  = fromIntegral $ from `mod` to'
    to'   = unit to


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
