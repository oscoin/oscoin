module Oscoin.Telemetry.Options
    ( Options(..)

    , telemetryOptionsParser
    , telemetryOptionsOpts
    , renderTelemetryOptionsOpts
    )
where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.Telemetry.Logging as Log

import qualified Formatting as F
import           Options.Applicative
import           System.Console.Option

data Options = Options
    { optLogSeverity :: Log.Severity
    , optLogStyle    :: Log.StyleFormatter
    } deriving (Eq, Show)

telemetryOptionsParser :: Parser Options
telemetryOptionsParser = Options
    <$> option auto
        ( long    "log-level"
       <> help    "Set the logging verbosity"
       <> metavar (enumBounded (Proxy @Log.Severity))
       <> value   Log.Debug
       <> showDefault
        )
    <*> styleParser
  where
    styleParser = Log.StyleFormatter
        <$> flag True False
            ( long    "no-log-colours"
           <> help    "Turn off terminal colors for log output"
            )
        <*> option auto
            ( long    "log-layout"
           <> help    "Control if and how to layout log messages"
           <> metavar (enumBounded (Proxy @Log.LayoutFormat))
           <> value   Log.HumanReadable
           <> showDefault
            )

    enumBounded
        :: forall proxy a. (Show a, Enum a, Bounded a)
        => proxy a
        -> String
    enumBounded _ = intercalate "|" $ map (show @a) [minBound..maxBound]

telemetryOptionsOpts :: Options -> [Opt Text]
telemetryOptionsOpts
    (Options
        optLogSeverity
        (Log.StyleFormatter useColours layoutFormat)) =
      Opt "log-level"  (show optLogSeverity)
    : Opt "log-layout" (show layoutFormat)
    : bool [] [Flag "no-log-colours"] (not useColours)

renderTelemetryOptionsOpts :: Options -> [Text]
renderTelemetryOptionsOpts = map (F.sformat F.build) . telemetryOptionsOpts
