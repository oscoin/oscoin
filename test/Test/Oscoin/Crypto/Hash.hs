module Test.Oscoin.Crypto.Hash
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto

import           Oscoin.Test.Crypto
import           Oscoin.Test.Util (Condensed(..))

import           Test.QuickCheck.Extended
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding ((===))

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson as Aeson

tests
    :: ( Show (Crypto.ShortHash c)
       , Show (Crypto.Hash c)
       , Arbitrary (Crypto.ShortHash c)
       , Arbitrary (Crypto.Hash c)
       ) => Dict (IsCrypto c) -> TestTree
tests d = testGroup "Oscoin.Crypto.Hash"
    [ testProperty "prop_jsonRoundtripHash" (prop_jsonRoundtripHash d)
    , testProperty "prop_jsonRoundtripShortHash" (prop_jsonRoundtripShortHash d)
    , testProperty "prop_textRoundtripShortHash" (prop_textRoundtripShortHash d)
    , testProperty "prop_serialiseRoundtripHash" (prop_serialiseRoundtripHash d)
    , testProperty "prop_serialiseRoundtripShortHash" (prop_serialiseRoundtripShortHash d)
    ]

prop_jsonRoundtripHash :: forall c. Dict (IsCrypto c) -> ByteString -> Property
prop_jsonRoundtripHash Dict bytes =
    let h = Crypto.hash @c bytes
    in (Aeson.decode . Aeson.encode) h === Just h

prop_jsonRoundtripShortHash :: forall c. Dict (IsCrypto c) -> Crypto.ShortHash c -> Property
prop_jsonRoundtripShortHash Dict h =
    (Aeson.decode . Aeson.encode) h === Just h

prop_textRoundtripShortHash
    :: forall c. Condensed (Crypto.ShortHash c)
    => Dict (IsCrypto c) -> Crypto.ShortHash c -> Property
prop_textRoundtripShortHash Dict h =
    (Crypto.parseShortHash . show) h === Just h

prop_serialiseRoundtripHash :: forall c. Dict (IsCrypto c) -> Crypto.Hash c -> Property
prop_serialiseRoundtripHash Dict h =
    (CBOR.deserialise . CBOR.serialise $ h) === h

prop_serialiseRoundtripShortHash :: forall c. Dict (IsCrypto c) -> Crypto.ShortHash c -> Property
prop_serialiseRoundtripShortHash Dict h =
    (CBOR.deserialise . CBOR.serialise $ h) === h
