module Test.Oscoin.Node.Options.Gen (genNodeOptions) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Hash (HasHashing)
import           Oscoin.Node.Options
import qualified Oscoin.P2P.Disco.Options as Disco
import           Oscoin.P2P.Types (renderHostname)
import           Oscoin.Time (seconds)

import qualified Test.Oscoin.Configuration.Gen as Config.Gen
import qualified Test.Oscoin.Crypto.Hash.Gen as Hash.Gen
import qualified Test.Oscoin.P2P.Disco.Options.Gen as Disco.Gen
import qualified Test.Oscoin.P2P.Gen as P2P.Gen

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genNodeOptions
    :: (HasHashing c, MonadGen m)
    => m (Options c Disco.OptNetwork)
genNodeOptions = Options
    <$> P2P.Gen.genIP
    <*> P2P.Gen.genPortNumber
    <*> P2P.Gen.genPortNumber
    <*> Disco.Gen.genOptions
    <*> Gen.word8
            (Range.constant 1 (fromIntegral $ Nakamoto.blockTime `div` seconds))
    <*> Config.Gen.paths
    <*> Config.Gen.environment
    <*> Gen.maybe (toS . renderHostname <$> P2P.Gen.genHostname)
    <*> Gen.maybe P2P.Gen.genPortNumber
    <*> Gen.maybe (toS . renderHostname <$> P2P.Gen.genHostname)
    <*> Gen.maybe P2P.Gen.genPortNumber
    <*> Gen.bool
    <*> Hash.Gen.genShortHash