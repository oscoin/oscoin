module Oscoin.Time
    ( Timestamp
    , Duration
    , prettyDuration
    , sinceEpoch
    , fromEpoch
    , timeAdd
    , timeDiff
    , epoch
    , now
    -- * Common durations
    , nanoseconds
    , microseconds
    , milliseconds
    , seconds
    , minutes
    , hours
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField (ToField)
import qualified Formatting as Fmt
import           System.Clock (Clock(Realtime), getTime, toNanoSecs)
import           System.Random (Random)

-- | A Duration represents the elapsed time between two instants as an 'Int64'
-- nanosecond count. The representation limits the largest representable
-- duration to approximately 290 years.
type Duration = Int64

-- | A Timestamp represents an absolute point in time.
newtype Timestamp = Timestamp Int64 deriving
    (Show, Read, Eq, Ord, Random, Bounded, Generic, Serialise, Fmt.Buildable)

deriving instance ToField Timestamp
deriving instance FromField Timestamp

prettyDuration :: Duration -> Text
prettyDuration d
    | d >= hours        = show (d `div` hours)        <> "h"
    | d >= minutes      = show (d `div` minutes)      <> "m"
    | d >= seconds      = show (d `div` seconds)      <> "s"
    | d >= milliseconds = show (d `div` milliseconds) <> "ms"
    | d >= microseconds = show (d `div` microseconds) <> "us"
    | otherwise         = show (d `div` nanoseconds)  <> "ns"

-- | UNIX epoch, yo.
epoch :: Timestamp
epoch = Timestamp 0

-- | Returns the 'Duration' since 'epoch' until the given 'Timestamp'.
sinceEpoch :: Timestamp -> Duration
sinceEpoch (Timestamp d) = d

-- | Returns a 'Timestamp' by adding the given 'Duration' to 'epoch'.
fromEpoch :: Duration -> Timestamp
fromEpoch = timeAdd epoch

-- | Returns a 'Timestamp' with the given 'Duration' added to it.
timeAdd :: Timestamp -> Duration -> Timestamp
timeAdd (Timestamp t) d = Timestamp (t + d)

-- | > timeDiff t t' = t - t'
timeDiff :: Timestamp -> Timestamp -> Duration
timeDiff (Timestamp t) (Timestamp t') = t - t'

-- | Returns the current 'Timestamp', relative to 'epoch'.
now :: MonadIO m => m Timestamp
now = liftIO $ Timestamp . fromIntegral . toNanoSecs <$> getTime Realtime

nanoseconds, microseconds, milliseconds, seconds, minutes, hours :: Duration
nanoseconds  = 1
microseconds = 1000 * nanoseconds
milliseconds = 1000 * microseconds
seconds      = 1000 * milliseconds
minutes      = 60   * seconds
hours        = 60   * minutes
