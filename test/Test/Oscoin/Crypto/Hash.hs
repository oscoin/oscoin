module Test.Oscoin.Crypto.Hash
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto

import           Oscoin.Test.Crypto

import           Test.QuickCheck.Extended
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding ((===))

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson as Aeson

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests d = testGroup "Oscoin.Crypto.Hash"
    [ testProperty "prop_jsonRoundtripHash" (prop_jsonRoundtripHash d)
    , testProperty "prop_serialiseRoundtripHash" (prop_serialiseRoundtripHash d)
    ]

prop_jsonRoundtripHash :: forall c. Dict (IsCrypto c) -> ByteString -> Property
prop_jsonRoundtripHash Dict bytes =
    let h = Crypto.hash @c bytes
    in (Aeson.decode . Aeson.encode) h === Just h

prop_serialiseRoundtripHash :: forall c. Dict (IsCrypto c) -> ByteString -> Property
prop_serialiseRoundtripHash Dict bytes =
    let h = Crypto.hash @c bytes
    in (CBOR.deserialise . CBOR.serialise $ h) === h
