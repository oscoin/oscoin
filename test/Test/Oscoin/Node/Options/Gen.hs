module Test.Oscoin.Node.Options.Gen (genNodeOptions) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (HasHashing)
import           Oscoin.Node.Options
import qualified Oscoin.P2P.Disco.Options as Disco
import           Oscoin.P2P.Types (renderHostname)

import qualified Test.Oscoin.Configuration.Gen as Config.Gen
import qualified Test.Oscoin.Crypto.Hash.Gen as Hash.Gen
import qualified Test.Oscoin.P2P.Disco.Options.Gen as Disco.Gen
import qualified Test.Oscoin.P2P.Gen as P2P.Gen
import qualified Test.Oscoin.Telemetry.Gen as Telemetry.Gen

import           Hedgehog
import qualified Hedgehog.Gen as Gen

genNodeOptions
    :: (HasHashing c, MonadGen m)
    => m (Options c Disco.OptNetwork)
genNodeOptions = Options
    <$> P2P.Gen.genIP
    <*> P2P.Gen.genPortNumber
    <*> P2P.Gen.genPortNumber
    <*> Disco.Gen.genOptions
    <*> Config.Gen.paths
    <*> Config.Gen.consensusOptions
    <*> Gen.maybe (toS . renderHostname <$> P2P.Gen.genHostname)
    <*> Gen.maybe P2P.Gen.genPortNumber
    <*> Gen.maybe (toS . renderHostname <$> P2P.Gen.genHostname)
    <*> Gen.maybe P2P.Gen.genPortNumber
    <*> Gen.bool
    <*> Hash.Gen.genShortHash
    <*> Telemetry.Gen.genTelemetryOptions
