{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Consensus.Nakamoto.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Consensus.Nakamoto (PoW(..))
import           Test.QuickCheck


instance Arbitrary PoW where
    arbitrary = PoW <$> arbitrary
