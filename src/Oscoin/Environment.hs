module Oscoin.Environment where

import           Oscoin.Prelude

data Environment = Production | Development | Testing
    deriving (Show, Enum, Bounded)

-- | A list of all the possible environments.
allEnvironments :: [Environment]
allEnvironments = [minBound .. maxBound]

-- | Renders the 'Environment' in a human-readable format.
prettyEnvironment :: Environment -> Text
prettyEnvironment Development = "development"
prettyEnvironment Production  = "production"
prettyEnvironment Testing     = "testing"
