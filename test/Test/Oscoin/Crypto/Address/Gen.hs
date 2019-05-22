module Test.Oscoin.Crypto.Address.Gen
    ( genAddress
    , genAddressFrom
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Address
import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.Crypto.PubKey
                 (HasDigitalSignature, PublicKey, generateKeyPair)
import           Oscoin.Test.Crypto.PubKey.Arbitrary

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified System.Random.SplitMix as SplitMix

genAddress
    :: (Hashable c (PublicKey c), MonadGen m)
    => (HasDigitalSignature c) => m (Address c)
genAddress = do
    seed <- Gen.word64 Range.constantBounded
    let gen = SplitMix.mkSMGen seed
    let (pk, _) = withFastRandomBytes gen generateKeyPair
    genAddressFrom pk

genAddressFrom :: (Hashable c (PublicKey c), MonadGen m) => PublicKey c -> m (Address c)
genAddressFrom pk = (`fromPublicKey` pk) <$> Gen.enumBounded
