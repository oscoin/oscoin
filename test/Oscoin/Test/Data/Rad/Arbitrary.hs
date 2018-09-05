module Oscoin.Test.Data.Rad.Arbitrary where

import           Oscoin.Prelude

import           Test.QuickCheck
import qualified Radicle as Rad

instance Arbitrary Rad.Value where
    arbitrary = pure $ Rad.String ""
