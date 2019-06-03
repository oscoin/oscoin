{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Test.Data.Tx.Arbitrary where

import           Oscoin.Crypto.Hash (fromHashed, hash)
import           Oscoin.Crypto.PubKey (PublicKey)
import qualified Oscoin.Data.OscoinTx as OscoinTx
import           Oscoin.Data.Tx
import           Oscoin.Prelude
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.PubKey.Arbitrary

import           Test.Oscoin.Data.OscoinTx (genTx)

import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.QuickCheck.Instances.ByteString ()

instance Arbitrary DummyPayload where
    arbitrary = DummyPayload <$> arbitrary

instance (IsCrypto c) => Arbitrary (Tx c) where
    arbitrary = do
        (pub :: PublicKey c, priv) <- arbitraryKeyPair
        msg           <- arbitrarySignedWith priv
        randomNonce   <- arbitrary
        randomChainId <- arbitrary
        randomHash    <- fromHashed . hash <$> (arbitrary :: Gen ByteString)
        pure $ (mkTx msg pub) { txNonce = randomNonce
                              , txChainId = randomChainId
                              , txContext = randomHash
                              }

instance (IsCrypto c) => Arbitrary (OscoinTx.Tx c) where
    arbitrary = hedgehog genTx
