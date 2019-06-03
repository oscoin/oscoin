module Test.Oscoin.Telemetry.Gen (genTelemetryOptions) where

import           Oscoin.Prelude

import           Oscoin.Telemetry.Logging (StyleFormatter(..))
import           Oscoin.Telemetry.Options

import           Hedgehog
import qualified Hedgehog.Gen as Gen

genTelemetryOptions :: MonadGen m => m Options
genTelemetryOptions = Options
    <$> Gen.enumBounded
    <*> (StyleFormatter <$> Gen.bool <*> Gen.enumBounded)
