module Test.Oscoin.Crypto.Address
    ( tests
    , props
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Address
import qualified Oscoin.Crypto.Address.Bech32 as Bech32
import           Oscoin.Crypto.Address.Serialisation
import           Oscoin.Crypto.PubKey

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitraryKeyPairFrom)
import           Oscoin.Test.Util (condensed)

import qualified Data.ByteString as BS
import           Data.ByteString.BaseN (Base(Base32z), decodeAtBase)
import           Data.IORef
import qualified Data.Set as Set
import           Formatting
import           System.IO.Unsafe (unsafePerformIO)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Gen.QuickCheck (quickcheck)
import qualified Hedgehog.Range as Range
import           Test.Oscoin.P2P.Handshake (genKeyPair)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog

tests :: forall c. Dict (IsCrypto c) -> [TestTree]
tests d@Dict =
    [ testGroup "Address"
      [ testGroup "Serialisation" [
          testProperty "deserializeAddress . serializeAddress == id" (propAddressSerdeRoundtrip d)
        , testProperty "decodeAddress . fmtAddress == id" (propRenderDecodeRoundtrip d)
        , testProperty "(fails) tampered address" (propInvalidChecksum d)
        ]
      , testProperty "Bech32.verifyChecksum . Bech32.createChecksum == id" propBech32checksumRoundtrip
      , testGroup "Injectivity" [
          testProperty "unique PKs map to unique addresses" (propInjectivity d)
        ]
      ]
   ]

props :: Dict (IsCrypto c) -> IO Bool
props d = checkParallel $ Group "Address.Serialisation"
    [ ("prop_address_serde_roundtrip",  propAddressSerdeRoundtrip d)
    , ("prop_render_decode_roundtrip",  propRenderDecodeRoundtrip d)
    , ("prop_invalid_protocol_version", propInvalidChecksum d)
    , ("prop_bech32_checksum_roundtrip", propBech32checksumRoundtrip)
    , ("prop_address_uniqueness", propInjectivity d)
    ]

propAddressSerdeRoundtrip :: forall c. Dict (IsCrypto c) -> Property
propAddressSerdeRoundtrip Dict = property $ do
    (pk, _) <- genKeyPair @c
    address <- forAll $ genAddress pk
    (deserializeAddress . serializeAddress $ address) === Right address

propRenderDecodeRoundtrip :: forall c. Dict (IsCrypto c) -> Property
propRenderDecodeRoundtrip Dict = property $ do
    (pk, _) <- genKeyPair @c
    address <- forAll $ genAddress pk
    let (b32 :: ByteString) = toS $ sformat fmtAddress address
    decodeAtBase Base32z b32 === Just (serializeAddress address)
    (decodeAddress . toS . sformat fmtAddress $ address) === Right address

propInvalidChecksum :: forall c. Dict (IsCrypto c) -> Property
propInvalidChecksum Dict = property $ do
    (pk, _) <- genKeyPair @c
    address <- forAll $ genAddress pk
    let serialized = serializeAddress address
    idx <- forAll (Gen.int (Range.linear 0 (BS.length serialized - 1)))
    w8  <- forAll (Gen.word8 Range.linearBounded)

    -- Change a random part of the address into something else.
    let tamper = uncurry mappend
               . bimap identity (BS.pack . (:) w8 . BS.unpack)
               . BS.splitAt idx

    invalidChecksum (deserializeAddress @c (tamper serialized)) === True
  where
    invalidChecksum (Left (InvalidChecksum _)) = True
    invalidChecksum _                          = False

propBech32checksumRoundtrip :: Property
propBech32checksumRoundtrip = property $ do
    hrp <- forAll (Gen.bytes $ Range.constantFrom 0 0 100)
    dat <- BS.unpack <$> forAll (Gen.bytes $ Range.constantFrom 0 0 100)
    (Bech32.verifyBech32checksum hrp . mappend dat . Bech32.bech32checksum hrp $ dat) === True


-- | Generate a bunch of addresses and make sure they are all unique. In
-- particular, assess that the transformation fromPublicKey(pk) is injective,
-- so that we don't end up with addresses which clashes.
--
-- NOTE(adn) Obviously this test is not /total/, as we cannot possibly test
-- all the infinite 'PublicKey's one might generate. However, it serve as a
-- smoke test to rule out we are not doing anything immediately silly when
-- creating new addresses. A more useful category of tests would be to use
-- a nightly CI to run this with the 'RealWorld' crypto for a large number
-- of keys.
propInjectivity :: forall c. Dict (IsCrypto c) -> Property
propInjectivity Dict = property $ do
    (seed, addresses) <- evalIO (readIORef addressesRef)
    (pk :: PublicKey Crypto, _) <- forAllWith (toS . condensed) (quickcheck (arbitraryKeyPairFrom seed))
    address <- renderAddress <$> forAll (genAddress pk)
    if Set.member address addresses
       then annotate ("Generated address not unique: " <> toS address) *> failure
       else do
           evalIO (atomicModifyIORef' addressesRef (\(!s, x) -> ((succ s, Set.insert address x), ())))
           success

{-# ANN addressesRef ("Hlint: ignore" :: String) #-}
addressesRef :: IORef (Word64, Set.Set Text)
addressesRef = unsafePerformIO $ newIORef (0, mempty)
{-# NOINLINE addressesRef #-}

{------------------------------------------------------------------------------
  Generators and other mythological creatures
------------------------------------------------------------------------------}

genAddress :: IsCrypto c => PublicKey c -> Gen (Address c)
genAddress pk = (`fromPublicKey` pk) <$> Gen.enumBounded
