{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Hash.Arbitrary where

import           Oscoin.Crypto.Hash
import           Oscoin.Prelude

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance (Arbitrary a, Hashable a) => Arbitrary (Hashed a) where
    arbitrary = hash <$> arbitrary
