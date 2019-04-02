module Test.Oscoin.Crypto.PubKey
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.PubKey.Arbitrary
                 (arbitraryKeyPair, arbitrarySigned)
import           Oscoin.Test.Util (condensed)

import           Test.QuickCheck.Extended
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding ((===))

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson as Aeson
import           Data.ByteArray.Orphans ()

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests d = testGroup "Oscoin.Crypto.PubKey"
    [ testProperty "prop_jsonRoundtripSigned" (prop_jsonRoundtripSigned d)
    , testProperty "prop_serialiseRoundtripSigned" (prop_serialiseRoundtripSigned d)
    , testProperty "prop_jsonRoundtripPublicKey" (prop_jsonRoundtripPublicKey d)
    , testProperty "prop_serialiseRoundtripPublicKey" (prop_serialiseRoundtripPublicKey d)
    , testProperty "prop_hashSigned" (prop_hashSigned d)
    ]

prop_jsonRoundtripSigned :: forall c. Dict (IsCrypto c) -> Property
prop_jsonRoundtripSigned Dict =
    forAll (arbitrarySigned Proxy :: Gen (Crypto.Signed c Text)) $ \msg ->
          (Aeson.eitherDecode . Aeson.encode) msg === Right msg

prop_serialiseRoundtripSigned :: forall c. Dict (IsCrypto c) -> Property
prop_serialiseRoundtripSigned Dict =
    forAll (arbitrarySigned Proxy :: Gen (Crypto.Signed c Text)) $ \msg ->
        (CBOR.deserialise . CBOR.serialise $ msg) === msg

prop_jsonRoundtripPublicKey :: forall c. Dict (IsCrypto c) -> Property
prop_jsonRoundtripPublicKey Dict =
    forAllShow (arbitraryKeyPair @c) (show . bimap identity condensed) $ \(pk, _) ->
        (Aeson.eitherDecode . Aeson.encode $ pk) === Right pk

prop_serialiseRoundtripPublicKey :: forall c. Dict (IsCrypto c) -> Property
prop_serialiseRoundtripPublicKey Dict =
    forAllShow (arbitraryKeyPair @c) (show . bimap identity condensed) $ \(pk, _) ->
        (CBOR.deserialise . CBOR.serialise $ pk) === pk

-- Verify that hashing a signed message is the same thing as hashing an
-- unsigned one.
prop_hashSigned :: forall c. Dict (IsCrypto c) -> Property
prop_hashSigned Dict =
    forAll (arbitrarySigned Proxy) $ \(signed :: Crypto.Signed c ByteString) ->
        let msg = Crypto.sigMessage signed
        in Crypto.fromHashed (Crypto.hash @c signed) === Crypto.fromHashed (Crypto.hash msg)
