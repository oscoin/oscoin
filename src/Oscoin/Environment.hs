module Oscoin.Environment where

import           Oscoin.Prelude

data Environment = Production | Development | Testing
    deriving (Show, Eq, Enum, Bounded)

-- | A list of all the possible environments.
allEnvironments :: [Environment]
allEnvironments = [minBound .. maxBound]

-- | Renders the 'Environment' into 'Text'.
toText :: Environment -> Text
toText Development = "development"
toText Production  = "production"
toText Testing     = "testing"

-- | Constructs an 'Environment' from a 'Text'.
fromText :: Text -> Maybe Environment
fromText "development" = Just Development
fromText "production"  = Just Production
fromText "testing"     = Just Testing
fromText _             = Nothing
