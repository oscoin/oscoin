{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.PubKey.Arbitrary (
      arbitrarySignedWith
    , arbitraryKeyPair
    , arbitraryKeyPairFrom
    , arbitraryKeyPairs
    , arbitrarySigned
    , withFastRandomBytes
    ) where

import           Oscoin.Test.Crypto

import           Oscoin.Crypto.PubKey
                 ( HasDigitalSignature
                 , PrivateKey
                 , PublicKey
                 , Signed
                 , generateKeyPair
                 , sign
                 )
import           Oscoin.Prelude

import           Crypto.Random.Types (MonadRandom(..))
import           Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteString as BS
import qualified System.Random.SplitMix as SplitMix

import           Test.QuickCheck


newtype FastRandomBytes a = FRB (StateT SplitMix.SMGen Identity a)
    deriving (Monad, Applicative, Functor, MonadState SplitMix.SMGen)

instance MonadRandom FastRandomBytes where
    getRandomBytes n = do
        smgen <- get
        let (bs, smgen') = BS.unfoldrN n gen smgen
        put $ fromMaybe smgen smgen'
        pure $ convert bs
      where
          gen :: SplitMix.SMGen -> Maybe (Word8, SplitMix.SMGen)
          gen g =
              let (i, g') = SplitMix.nextInt g
              in Just (fromIntegral i, g')

withFastRandomBytes :: SplitMix.SMGen -> FastRandomBytes a -> a
withFastRandomBytes smgen (FRB m) = runIdentity $ evalStateT m smgen

arbitrarySignedWith
    :: ( Arbitrary a
       , ByteArrayAccess a
       , HasDigitalSignature c
       )
    => PrivateKey c
    -> Gen (Signed c a)
arbitrarySignedWith pk = do
    seed <- arbitrary
    withFastRandomBytes (SplitMix.mkSMGen seed) . sign pk <$> arbitrary

-- | Generate a key pair.
--
-- Note that using this more than once in a property will lead to duplicate
-- keypairs, as the random seed is independent. Use 'arbitraryKeyPairs' if you
-- need more than one.
arbitraryKeyPair
    :: (HasHashing c, HasDigitalSignature c)
    => Gen (PublicKey c, PrivateKey c)
arbitraryKeyPair =
    flip withFastRandomBytes generateKeyPair <$> genSeed

-- | Generates a new key pair from a given 'Word64` seed.
arbitraryKeyPairFrom
    :: (HasHashing c, HasDigitalSignature c)
    => Word64
    -> Gen (PublicKey c, PrivateKey c)
arbitraryKeyPairFrom seed =
    pure $ withFastRandomBytes (SplitMix.mkSMGen seed) generateKeyPair

-- | Generate @n@ distinct key pairs.
arbitraryKeyPairs
    :: (HasHashing c, HasDigitalSignature c)
    => Int
    -> Gen [(PublicKey c, PrivateKey c)]
arbitraryKeyPairs n =
    flip withFastRandomBytes (replicateM n generateKeyPair) <$> genSeed

arbitrarySigned
    :: forall a c.
       ( ByteArrayAccess a
       , Arbitrary a
       , IsCrypto c
       ) => Proxy c
         -> Gen (Signed c a)
arbitrarySigned Proxy = do
    (_, priv :: PrivateKey c) <- arbitraryKeyPair
    arbitrarySignedWith priv

-- Internal --------------------------------------------------------------------

genSeed :: Gen SplitMix.SMGen
genSeed = SplitMix.mkSMGen <$> arbitrary
