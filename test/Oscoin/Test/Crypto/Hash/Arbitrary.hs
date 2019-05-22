{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Hash.Arbitrary where

import           Oscoin.Crypto.Hash
import           Oscoin.Prelude

import           Test.QuickCheck

instance HasHashing c => Arbitrary (Hash c) where
    arbitrary =
        fromHashed . hashBinary <$> (arbitrary :: Gen String)

instance HasHashing c => Arbitrary (ShortHash c) where
    arbitrary =
        toShortHash <$> arbitrary
