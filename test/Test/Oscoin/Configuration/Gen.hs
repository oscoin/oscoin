module Test.Oscoin.Configuration.Gen
    ( paths
    , environment
    , filePath
    )
where

import           Oscoin.Prelude

import           Oscoin.Configuration (Environment, Paths(..))

import           System.FilePath ((</>))

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

paths :: MonadGen m => m Paths
paths = Paths <$> filePath <*> filePath <*> filePath

environment :: MonadGen m => m Environment
environment = Gen.enumBounded

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
