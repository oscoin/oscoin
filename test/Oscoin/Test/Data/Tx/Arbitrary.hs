{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Data.Tx.Arbitrary where

import           Oscoin.Prelude
import           Oscoin.Data.Tx
import           Oscoin.Crypto.Hash (hash)
import           Oscoin.Test.Crypto.PubKey.Arbitrary

import           Test.QuickCheck
import           Data.Binary (Binary)

instance (Binary a, Arbitrary a) => Arbitrary (Tx a) where
    arbitrary = do
        (pub, priv) <- arbitraryKeyPair
        msg         <- arbitrarySignedWith priv
        pure $ mkTx msg (hash pub)
