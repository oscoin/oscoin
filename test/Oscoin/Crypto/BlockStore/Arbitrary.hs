{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Crypto.BlockStore.Arbitrary where

import Oscoin.Prelude

import Oscoin.Crypto.Blockchain.Arbitrary
import Oscoin.Consensus.BlockStore

import Data.Binary (Binary)

import Test.QuickCheck

instance (Binary tx, Arbitrary tx, Monoid s) => Arbitrary (BlockStore tx s) where
    arbitrary =
        genesisBlockStore <$> arbitraryGenesis
