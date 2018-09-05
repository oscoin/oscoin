{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.PubKey.Arbitrary where

import Oscoin.Prelude
import Oscoin.Crypto.PubKey (Signed, PublicKey, PrivateKey, Signed, sign, generateKeyPair)

import Crypto.Random.Types (MonadRandom(..))
import Data.ByteArray (convert)
import Codec.Serialise (Serialise)

import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance MonadRandom Gen where
    getRandomBytes n =
        convert <$> resize n (arbitrary :: Gen ByteString)

arbitrarySignedWith
    :: (Arbitrary a, Serialise a) => PrivateKey -> Gen (Signed a)
arbitrarySignedWith pk =
    arbitrary >>= sign pk

arbitraryKeyPair :: Gen (PublicKey, PrivateKey)
arbitraryKeyPair = generateKeyPair

arbitrarySigned :: (Serialise a, Arbitrary a) => Gen (Signed a)
arbitrarySigned = do
    (_, priv) <- arbitraryKeyPair
    arbitrarySignedWith priv
