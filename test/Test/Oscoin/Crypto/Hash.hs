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
import           Data.ByteArray.Orphans ()
import qualified Data.ByteString as BS

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests d = testGroup "Oscoin.Crypto.Hash"
    [ testProperty "JSON serialization of 'Hashed'" (propJsonHashRoundtrip d)
    , testProperty "CBOR serialization of 'Hashed'" (propSerialiseHashRoundtrip d)
    ]

propJsonHashRoundtrip :: forall c. Dict (IsCrypto c) -> ByteString -> Bool
propJsonHashRoundtrip Dict bs =
    let x = Crypto.hash @c bs
     in (Aeson.decode . Aeson.encode) x == Just x

propSerialiseHashRoundtrip :: forall c. Dict (IsCrypto c) -> Property
propSerialiseHashRoundtrip Dict =
    forAll (arbitrary @ByteString) $ \bytes ->
        (BS.length bytes > 0) ==>
            let h = Crypto.hash @c bytes
            in (CBOR.deserialise . CBOR.serialise $ h) === h
