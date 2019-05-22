module Test.Oscoin.Crypto.Hash.Gen
    ( genHash
    , genShortHash
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash

import           Hedgehog
import qualified Hedgehog.Gen as Gen

genHash :: forall m c. MonadGen m => HasHashing c => m (Hash c)
genHash = do
    w <- Gen.enumBounded :: m Word32
    pure $ fromHashed $ hashBinary @c w

genShortHash :: MonadGen m => HasHashing c => m (ShortHash c)
genShortHash =
    toShortHash <$> genHash
