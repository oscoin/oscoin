module Oscoin.Consensus.Simple.Arbitrary where

import           Oscoin.Prelude
import           Oscoin.Consensus.Simple

import           Test.QuickCheck

instance (Arbitrary tx) => Arbitrary (NodeMsg tx) where
    arbitrary = ClientTx <$> arbitrary
