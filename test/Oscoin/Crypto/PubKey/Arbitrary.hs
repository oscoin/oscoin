{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Crypto.PubKey.Arbitrary where

import Oscoin.Prelude
import Oscoin.Crypto.PubKey (PrivateKey, Signed, sign)

import Crypto.Random.Types (MonadRandom(..))
import Data.ByteArray (convert)
import Data.Binary (Binary)

import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance MonadRandom Gen where
    getRandomBytes n =
        convert <$> resize n (arbitrary :: Gen ByteString)

arbitrarySignedWith
    :: forall tx. (Arbitrary tx, Binary tx)
    => PrivateKey
    -> Gen (Signed tx)
arbitrarySignedWith pk =
    arbitrary >>= sign pk
