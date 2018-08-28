{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.BlockStore.Arbitrary where

import Oscoin.Prelude

import Oscoin.Test.Crypto.Blockchain.Arbitrary
import Oscoin.Consensus.BlockStore

import Data.Binary (Binary)

import Test.QuickCheck

instance (Binary tx, Arbitrary tx, Default s) => Arbitrary (BlockStore tx s) where
    arbitrary =
        genesisBlockStore <$> arbitraryGenesis
