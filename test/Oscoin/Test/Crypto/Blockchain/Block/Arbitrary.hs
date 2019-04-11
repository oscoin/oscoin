{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Test.Crypto.Blockchain.Block.Arbitrary
    () where

import           Codec.Serialise (Serialise)

import           Oscoin.Crypto.Blockchain

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()
import           Test.QuickCheck

instance Arbitrary s => Arbitrary (Sealed c s) where
    arbitrary = genSeal

instance ( Serialise tx
         , Serialise s
         , Arbitrary tx
         , Arbitrary s
         , IsCrypto c
         ) => Arbitrary (Block c tx s) where
    arbitrary = genStandaloneBlock
