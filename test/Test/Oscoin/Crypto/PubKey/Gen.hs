module Test.Oscoin.Crypto.PubKey.Gen
    ( genKeyPair
    , genPublicKey
    , genSignature
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (HasHashing)
import           Oscoin.Crypto.PubKey
                 ( HasDigitalSignature
                 , PrivateKey
                 , PublicKey
                 , Signature
                 , Signed(..)
                 , generateKeyPair
                 , sign
                 )
import           Oscoin.Test.Crypto.PubKey.Arbitrary

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified System.Random.SplitMix as SplitMix

genKeyPair
    :: forall c m. (HasHashing c, HasDigitalSignature c, MonadGen m)
    => m (PublicKey c, PrivateKey c)
genKeyPair = do
    seed <- Gen.word64 Range.constantBounded
    let gen = SplitMix.mkSMGen seed
    pure $ withFastRandomBytes gen (generateKeyPair @c)

genPublicKey
    :: forall c m. (HasHashing c, HasDigitalSignature c, MonadGen m)
    => m (PublicKey c)
genPublicKey =
    fst <$> genKeyPair

genSignature
    :: forall c m. (HasHashing c, HasDigitalSignature c, MonadGen m)
    => m (Signature c)
genSignature = do
    msg  <- Gen.bytes (Range.constant 1 32)
    seed <- Gen.word64 Range.constantBounded
    pure . withFastRandomBytes (SplitMix.mkSMGen seed) $ do
        (_, sk) <- generateKeyPair @c
        sigSignature <$> sign @c sk msg
