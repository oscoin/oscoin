{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Storage.Block.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Storage.Block (BlockStore)
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Test.Crypto.Blockchain.Arbitrary

import           Codec.Serialise (Serialise)

import           Test.QuickCheck

instance (Serialise tx, Arbitrary tx, Arbitrary s) => Arbitrary (BlockStore tx s) where
    arbitrary =
        BlockStore.initWithChain <$> arbitraryBlockchain
