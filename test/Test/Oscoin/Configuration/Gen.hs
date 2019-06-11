module Test.Oscoin.Configuration.Gen
    ( paths
    , consensusOptions
    , filePath
    )
where

import           Oscoin.Prelude

import           Oscoin.Configuration
                 (ConsensusOptions(..), NakamotoLenientOptions(..), Paths(..))
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Time (seconds)

import           System.FilePath ((</>))

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

paths :: MonadGen m => m Paths
paths = Paths <$> filePath <*> filePath <*> filePath

consensusOptions :: forall m. MonadGen m => m ConsensusOptions
consensusOptions = Gen.choice [genNakamotoStrict, genNakamotoLenient]
  where
      genNakamotoStrict :: m ConsensusOptions
      genNakamotoStrict = pure NakamotoStrict

      genNakamotoLenient :: m ConsensusOptions
      genNakamotoLenient = do
          blockTime <- Gen.word8
            (Range.constant 1 (fromIntegral $ Nakamoto.blockTime `div` seconds))
          NakamotoLenient . NakamotoLenientOptions blockTime <$> Gen.bool

-- nb. ranges are way below admissible values for performance reasons
filePath :: MonadGen m => m FilePath
filePath = do
    root <- Gen.element ["", "/"]
    dirs <- Gen.list (Range.constantFrom 1 1 23) dir
    pure $ case dirs of
        [] -> root
        xs -> foldl' (</>) root xs
  where
    dir = Gen.string (Range.constantFrom 1 1 42) Gen.latin1
