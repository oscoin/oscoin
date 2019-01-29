{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Storage.Block.Arbitrary where

import           Oscoin.Prelude

import qualified Oscoin.Storage.Block.Pure as Pure
import           Oscoin.Test.Crypto.Blockchain.Arbitrary

import           Codec.Serialise (Serialise)

import           Test.QuickCheck

instance (Serialise tx, Arbitrary tx) => Arbitrary (Pure.Handle tx ()) where
    arbitrary =
        Pure.initWithChain <$> arbitraryBlockchain
