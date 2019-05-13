{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Test.Crypto.Blockchain.Block.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitraryKeyPair)
import           Oscoin.Test.Time ()

import           Codec.Serialise (Serialise)

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

instance
    forall tx c.
    ( IsCrypto c
    , Arbitrary tx
    , Serialise tx
    )
    => Arbitrary (BlockData c tx)
  where
    arbitrary = do
        (pk, _) <- arbitraryKeyPair
        txs     <- arbitrary :: Gen [tx]
        pure $ mkBlockData pk txs
