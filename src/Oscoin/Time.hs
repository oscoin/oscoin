module Oscoin.Time
    ( Timestamp
    , Duration
    , sinceEpoch
    , fromEpoch
    , timeAdd
    , timeDiff
    , epoch
    , now
    , nanoseconds
    , microseconds
    , milliseconds
    , seconds
    , minutes
    , hours
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Formatting as Fmt
import           System.Clock (Clock(Realtime), getTime, toNanoSecs)
import           System.Random (Random)

-- | A Duration represents the elapsed time between two instants
-- as an Int64 nanosecond count. The representation limits the
-- largest representable duration to approximately 290 years.
type Duration = Int64

-- | A Timestamp represents an absolute point in time.
newtype Timestamp = Timestamp Int64 deriving
    (Show, Read, Eq, Ord, Random, Bounded, Generic,
        ToJSON, FromJSON, Serialise, Fmt.Buildable)

-- | UNIX epoch, yo.
epoch :: Timestamp
epoch = Timestamp 0

-- | Returns the Duration since 'epoch' until the given Timestamp.
sinceEpoch :: Timestamp -> Duration
sinceEpoch (Timestamp d) = d

-- | Returns a 'Timestamp' by adding the given 'Duration' to 'epoch'.
fromEpoch :: Duration -> Timestamp
fromEpoch = timeAdd epoch

-- | Returns a Timestamp with the given Duration added to it.
timeAdd :: Timestamp -> Duration -> Timestamp
timeAdd (Timestamp t) d = Timestamp (t + d)

-- | @ t `timeDiff` t' @ gives the duration elapsed at @t@ since @t'@
timeDiff :: Timestamp -> Timestamp -> Duration
timeDiff (Timestamp t) (Timestamp t') = t - t'

-- | Returns the current Timestamp
now :: MonadIO m => m Timestamp
now = liftIO $ Timestamp . fromIntegral . toNanoSecs <$> getTime Realtime

-- | Common durations.
nanoseconds, microseconds, milliseconds, seconds, minutes, hours :: Duration
nanoseconds  = 1
microseconds = 1000 * nanoseconds
milliseconds = 1000 * microseconds
seconds      = 1000 * milliseconds
minutes      = 60   * seconds
hours        = 60   * minutes
