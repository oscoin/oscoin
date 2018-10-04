{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.BlockStore.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (BlockStore)
import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Test.Crypto.Blockchain.Arbitrary

import           Codec.Serialise (Serialise)

import           Test.QuickCheck

instance (Serialise tx, Arbitrary tx, Arbitrary s) => Arbitrary (BlockStore tx s) where
    arbitrary =
        BlockStore.initWithChain <$> arbitraryBlockchain
