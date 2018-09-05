{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.BlockStore.Arbitrary where

import Oscoin.Prelude

import Oscoin.Test.Crypto.Blockchain.Arbitrary
import Oscoin.Consensus.BlockStore

import Codec.Serialise (Serialise)

import Test.QuickCheck

instance (Serialise tx, Arbitrary tx, Default s) => Arbitrary (BlockStore tx s) where
    arbitrary =
        genesisBlockStore <$> arbitraryGenesis
