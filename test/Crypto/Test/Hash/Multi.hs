module Crypto.Test.Hash.Multi
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Crypto.Hash as C
import           Crypto.Hash.Multi (Multihashable)
import qualified Crypto.Hash.Multi as Multihash
import           Data.ByteString.BaseN (Base, DecodeBase)
import qualified Data.ByteString.BaseN as BaseN

import           Hedgehog (MonadGen, PropertyT, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests = testGroup "Multihash"
    [ testGroup "Examples from https://github.com/multiformats/multihash"
        [ testGroup "sha1(\"multihash\")"
            [ testCase "Base16" $
                example BaseN.Base16 C.SHA1 "multihash"
                    "111488c2f11fb2ce392acb5b2986e640211c4690073e"
            , testCase "Base58" $
                example BaseN.Base58 C.SHA1 "multihash"
                    "5dsgvJGnvAfiR3K6HCBc4hcokSfmjj"
            , testCase "Base64" $
                example BaseN.Base64 C.SHA1 "multihash"
                    "ERSIwvEfss45KstbKYbmQCEcRpAHPg=="
            ]
        , testGroup "sha2-256(\"multihash\")"
            [ testCase "Base16" $
                example BaseN.Base16 C.SHA256 "multihash"
                    "12209cbc07c3f991725836a3aa2a581ca2029198aa420b9d99bc0e131d9f3e2cbe47"
            , testCase "Base58" $
                example BaseN.Base58 C.SHA256 "multihash"
                    "QmYtUc4iTCbbfVSDNKvtQqrfyezPPnFvE33wFmutw9PBBk"
            , testCase "Base64" $
                example BaseN.Base64 C.SHA256 "multihash"
                    "EiCcvAfD+ZFyWDajqipYHKICkZiqQgudmbwOEx2fPiy+Rw=="
            ]
        ]

    , testGroup "Roundtrip"
        [ testProperty "Base16: decode . encode = id" . property $ do
            bs <- forAll genBytes
            propRoundtrip BaseN.Base16 C.Blake2b_256 bs

        , testProperty "Base58: decode . encode = id" . property $ do
            bs <- forAll genBytes
            propRoundtrip BaseN.Base58 C.Blake2b_256 bs

        , testProperty "Base64: decode . encode = id" . property $ do
            bs <- forAll genBytes
            propRoundtrip BaseN.Base64 C.Blake2b_256 bs
        ]
    ]

propRoundtrip
    :: ( Multihashable a
       , DecodeBase    b
       )
    => Base b
    -> a
    -> ByteString
    -> PropertyT IO ()
propRoundtrip base algo bs =
    let digest = C.hashWith algo bs
        enc    = Multihash.encodeAtBase base . Multihash.fromDigest
        dec    = Multihash.decodeAtBase base . BaseN.encodedBytes
     in (dec . enc) digest === Right digest

example
    :: Multihashable a
    => BaseN.Base b
    -> a
    -> ByteString
    -> ByteString
    -> Assertion
example base algo input expected =
    BaseN.encodedBytes (multihash base algo input) @?= expected

--------------------------------------------------------------------------------

genBytes :: MonadGen m => m ByteString
genBytes = Gen.utf8 (Range.constantFrom 8 8 512) Gen.unicodeAll

multihash
    :: Multihashable a
    => BaseN.Base b
    -> a
    -> ByteString
    -> BaseN.AtBase b
multihash base algo bs =
    Multihash.encodeAtBase base $ Multihash.multihash algo bs
