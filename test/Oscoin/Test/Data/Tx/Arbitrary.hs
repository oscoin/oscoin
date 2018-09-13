{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Data.Tx.Arbitrary where

import           Oscoin.Prelude
import           Oscoin.Data.Tx
import           Oscoin.Test.Crypto.PubKey.Arbitrary

import           Test.QuickCheck
import           Codec.Serialise (Serialise)

instance (Serialise a, Arbitrary a) => Arbitrary (Tx a) where
    arbitrary = do
        (pub, priv) <- arbitraryKeyPair
        msg         <- arbitrarySignedWith priv
        pure $ mkTx msg pub
