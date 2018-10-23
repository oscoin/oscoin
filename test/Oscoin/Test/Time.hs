{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Time where

import           Oscoin.Prelude
import           Oscoin.Time

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary Timestamp where
    arbitrary = fromEpoch <$> (arbitrary `suchThat` (>= 0))
