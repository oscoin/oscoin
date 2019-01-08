module Oscoin.Environment where

import           Oscoin.Prelude

data Environment = Production | Development | Testing
    deriving (Show, Enum, Bounded)

-- | A list of all the possible environments.
allEnvironments :: [Environment]
allEnvironments = [minBound .. maxBound]
