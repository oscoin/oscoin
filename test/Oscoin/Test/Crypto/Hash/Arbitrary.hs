{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Hash.Arbitrary where

import           Oscoin.Crypto.Hash
import           Oscoin.Prelude

import           Test.QuickCheck

instance Arbitrary Hash where
    arbitrary =
        fromHashed . hashBinary <$> (arbitrary :: Gen String)

