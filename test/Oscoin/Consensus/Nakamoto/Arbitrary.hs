module Oscoin.Consensus.Nakamoto.Arbitrary where

import Oscoin.Prelude
import Oscoin.Consensus.Nakamoto

import Test.QuickCheck

instance Arbitrary tx => Arbitrary (NodeMsg tx) where
    arbitrary = TxMsg <$> arbitrary
