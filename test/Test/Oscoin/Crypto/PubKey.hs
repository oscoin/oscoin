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
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck hiding ((===))

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson as Aeson
import           Data.ByteArray.Orphans ()

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests d = testGroup "Oscoin.Crypto.PubKey"
    [ testProperty "JSON serialization of 'Signed'" (propSignedJSON d)
    , testProperty "CBOR serialization of 'Signed'" (testSerialiseSignedRoundtrip d)
    , testProperty "JSON serialization of key pair" (testJsonPkRoundtrip d)
    , testProperty "CBOR serialization of key pair" (testSerialisePkRoundtrip d)
    , testCase "Hash of signed message is hash of message" (testHashSigned d)
    ]

propSignedJSON :: forall c. Dict (IsCrypto c) -> Property
propSignedJSON Dict =
    forAll (arbitrarySigned (Proxy @c) :: Gen (Crypto.Signed c Text)) $ \msg ->
          (Aeson.eitherDecode . Aeson.encode) msg === Right msg

testJsonPkRoundtrip :: forall c. Dict (IsCrypto c) -> Property
testJsonPkRoundtrip Dict =
    forAllShow (arbitraryKeyPair @c) (show . bimap identity condensed) $ \(pk, _) ->
        (Aeson.eitherDecode . Aeson.encode $ pk) === Right pk

testSerialisePkRoundtrip :: forall c. Dict (IsCrypto c) -> Property
testSerialisePkRoundtrip Dict =
    forAllShow (arbitraryKeyPair @c) (show . bimap identity condensed) $ \(pk, _) ->
        (CBOR.deserialise . CBOR.serialise $ pk) === pk

testSerialiseSignedRoundtrip :: forall c. Dict (IsCrypto c) -> Property
testSerialiseSignedRoundtrip Dict =
    forAll (arbitrarySigned (Proxy @c) :: Gen (Crypto.Signed c Text)) $ \msg ->
        (CBOR.deserialise . CBOR.serialise $ msg) === msg

-- Verify that hashing a signed message is the same thing as hashing an
-- unsigned one.
testHashSigned :: forall c. Dict (IsCrypto c) -> Assertion
testHashSigned Dict = do
    let val :: Text = "fnord"
    (_, priKey) <- Crypto.generateKeyPair @c
    signed <- Crypto.sign priKey val

    Crypto.fromHashed (Crypto.hash @c signed) @?=
        Crypto.fromHashed (Crypto.hash val)
