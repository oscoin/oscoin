module Test.Oscoin.Crypto.Address.Gen
    ( genAddress
    , genAddressFrom
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Address
import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.Crypto.PubKey (HasDigitalSignature, PublicKey)

import           Test.Oscoin.Crypto.PubKey.Gen (genKeyPair)

import           Hedgehog
import qualified Hedgehog.Gen as Gen

genAddress
    :: (Hashable c (PublicKey c), MonadGen m)
    => (HasDigitalSignature c) => m (Address c)
genAddress = do
    (pk, _) <- genKeyPair
    genAddressFrom pk

genAddressFrom :: (Hashable c (PublicKey c), MonadGen m) => PublicKey c -> m (Address c)
genAddressFrom pk = (`fromPublicKey` pk) <$> Gen.enumBounded
