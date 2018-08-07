{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Crypto.Hash.Arbitrary where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance (Arbitrary a, Hashable a) => Arbitrary (Hashed a) where
    arbitrary = hash <$> arbitrary
