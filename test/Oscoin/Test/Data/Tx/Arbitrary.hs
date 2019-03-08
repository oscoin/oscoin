{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Test.Data.Tx.Arbitrary where

import           Oscoin.Crypto.Hash (fromHashed, hash)
import           Oscoin.Crypto.PubKey (PK)
import           Oscoin.Data.Tx
import           Oscoin.Prelude
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.PubKey.Arbitrary

import           Data.ByteArray (ByteArrayAccess)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

instance (IsCrypto c, ByteArrayAccess a, Arbitrary a) => Arbitrary (Tx c a) where
    arbitrary = do
        (pub :: PK c, priv) <- arbitraryKeyPair
        msg           <- arbitrarySignedWith priv
        randomNonce   <- arbitrary
        randomChainId <- arbitrary
        randomHash    <- fromHashed . hash <$> (arbitrary :: Gen ByteString)
        pure $ (mkTx msg pub) { txNonce = randomNonce
                              , txChainId = randomChainId
                              , txContext = randomHash
                              }
