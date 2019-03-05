{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.PubKey.Arbitrary where

import           Oscoin.Test.Crypto

import           Oscoin.Crypto.PubKey
                 (HasDigitalSignature, PK, SK, Signed, generateKeyPair, sign)
import           Oscoin.Prelude

import           Crypto.Random.Types (MonadRandom(..))
import           Data.ByteArray (ByteArrayAccess, convert)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance MonadRandom Gen where
    getRandomBytes n =
        convert <$> resize n (arbitrary :: Gen ByteString)

arbitrarySignedWith
    :: ( Arbitrary a
       , ByteArrayAccess a
       , HasDigitalSignature c
       )
    => SK c
    -> Gen (Signed c a)
arbitrarySignedWith pk =
    arbitrary >>= sign pk

arbitraryKeyPair :: (HasHashing c, HasDigitalSignature c) => Gen (PK c, SK c)
arbitraryKeyPair = generateKeyPair

arbitrarySigned
    :: forall a c.
       ( ByteArrayAccess a
       , Arbitrary a
       , IsCrypto c
       ) => Proxy c
         -> Gen (Signed c a)
arbitrarySigned Proxy = do
    (_, priv :: SK c) <- arbitraryKeyPair
    arbitrarySignedWith priv
