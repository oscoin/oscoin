{-# LANGUAGE UndecidableInstances #-}
module Test.Oscoin.Crypto.Address
    ( tests
    , props
    , genAddress
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Address
import qualified Oscoin.Crypto.Address.Bech32 as Bech32
import           Oscoin.Crypto.Address.Internal
import           Oscoin.Crypto.Address.Serialisation
import           Oscoin.Crypto.PubKey

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitraryKeyPairFrom)
import           Oscoin.Test.Util (condensed)

import           Codec.CBOR.Decoding as CBOR
import           Codec.Serialise
import           Control.Monad.Except (liftEither, throwError)
import           Control.Monad.Fail (fail)
import           Data.Binary.Decoding
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
        , testProperty "(works) rudimentary migration" (propMigration d)
        , testProperty "(check) addressLength" (propLengthCheck d)
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
    , ("prop_address_length_check", propLengthCheck d)
    ]

-- | This 'Property' tests that serializing & deserializing an Address
-- equals 'identity'.
propAddressSerdeRoundtrip :: forall c. Dict (IsCrypto c) -> Property
propAddressSerdeRoundtrip Dict = property $ do
    (pk, _) <- genKeyPair @c
    address <- forAll $ genAddress pk
    (deserializeAddress . serializeAddress $ address) === Right address

-- | This 'Property' tests that rendering & decoding a serialized address
-- equals 'identity'.
propRenderDecodeRoundtrip :: forall c. Dict (IsCrypto c) -> Property
propRenderDecodeRoundtrip Dict = property $ do
    (pk, _) <- genKeyPair @c
    address <- forAll $ genAddress pk
    let (b32 :: ByteString) = toS $ sformat fmtAddress address
    decodeAtBase Base32z b32 === Just (serializeAddress address)
    (decodeAddress . toS . sformat fmtAddress $ address) === Right address

-- | This 'Property' tests that if we tamper a serialised 'Address' the
-- checksum validation will catch it.
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

-- | This 'Property' tests the 'Bech32' checksum.
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
  Testing migrations
------------------------------------------------------------------------------}

-- Testing that we can migrate from different representation of an 'Address'
-- is important to future-proof our system. To do so, we 'newtype' wrap our
-- current 'Address' , simulating what could happen in an hypothetical future.
-- Then, we test that we can parse both the old and the new format. While not
-- thorough, this test gives a minimal degree of confidence on the soundness
-- of our migration strategy.

data FutureAddress c = FutureAddress
    { futureAddressPrefix  :: FutureAddressPrefix
    , futureAddressPayload :: FutureAddressPayload c
    }

deriving instance Eq (PublicKey c)   => Eq (FutureAddress c)
deriving instance Ord (PublicKey c)  => Ord (FutureAddress c)
deriving instance Show (PublicKey c) => Show (FutureAddress c)

-- The world has evolved and we ended up consuming all our reserved bytes.
data FutureAddressPrefix = FutureAddressPrefix
    { legacyPrefix :: AddressPrefix
    , fooByte      :: Word8
    -- ^ A byte reserved for \"foo\" things.
    , barByte      :: Word8
    -- ^ A byte reserved for \"bar\" things.
    } deriving (Eq, Ord, Show)

-- Let's assume we changed our 'AddressPayload' to also add an account index
-- and an address index as we switched to BIP32 and HD-wallets.

type AccountIndex = Word8
type AddressIndex = Word8

data FutureAddressPayload c where
    FutureAddressPayload_V0 :: PublicKey c -> FutureAddressPayload c
    FutureAddressPayload_V1 :: AccountIndex -> AddressIndex -> PublicKey c -> FutureAddressPayload c

deriving instance Eq (PublicKey c)   => Eq (FutureAddressPayload c)
deriving instance Ord (PublicKey c)  => Ord (FutureAddressPayload c)
deriving instance Show (PublicKey c) => Show (FutureAddressPayload c)

-- Note how we don't write how to serialize the new future address, but
-- rather we specify how to /deserialize/ it only. Then, in the tests, we will
-- serialize an \"old\" 'Address' but try to use the new deserialiser, checking
-- the binary format is still compatible (and that writing such migration
-- schemes is possible).

deserializeFutureAddress
    :: Serialise (PublicKey c)
    => ByteString
    -> Either DeserializeError (FutureAddress c)
deserializeFutureAddress blob = flip evalState blob . runExceptT $ do
    crc <- Checksum <$> withExceptT DecodeError (consumeFromEnd 6)
    protectedData <- get

    unless (isValidChecksum protectedData crc) $
        throwError (InvalidChecksum crc)

    legacyPrefix <- deserializeAddressPrefix
    fooByte <- withExceptT DecodeError getWord8
    barByte <- withExceptT DecodeError getWord8
    let prefix = FutureAddressPrefix legacyPrefix fooByte barByte

    FutureAddress <$> pure prefix <*> deserializeAddressPayload (addressFormat legacyPrefix)
  where
    deserializeAddressPayload (AddressFormat tag) = do
        binaryBlob <- get
        case tag of
          AddressFormatTag_CBOR ->
              liftEither $
                  first DeserializePayloadError
                        (deserialiseOrFail (toS binaryBlob))

-- Simulates what we would write when migrating. In practice, this would
-- supersede the 'Serialise' instance we current have for 'Address'.
instance Serialise (PublicKey c) => Serialise (FutureAddressPayload c) where
    encode _ = panic "out of scope for the current test"
    decode = do
        decodeListLenCanonicalOf 2
        tag <- decodeWordCanonical
        case tag of
          0 -> FutureAddressPayload_V0     <$> decode
          1 -> FutureAddressPayload_V1 0 0 <$> decode
          _ -> fail $ "Invalid tag when decoding an AddressPayload: " <> show tag

propMigration :: forall c. Dict (IsCrypto c) -> Property
propMigration Dict = property $ do
    (pk, _) <- genKeyPair @c
    oldAddress <- forAll $ genAddress pk
    let futureAddress = FutureAddress
          { futureAddressPrefix = FutureAddressPrefix (addressPrefix oldAddress) 0 0
          , futureAddressPayload = FutureAddressPayload_V0 pk
          }
    (deserializeFutureAddress . serializeAddress $ oldAddress) === Right futureAddress

-- Simple smoke test which purpose is to fail whenever we change the address
-- serialisation structure, to make us reason on the implications.
propLengthCheck :: forall c. Dict (IsCrypto c) -> Property
propLengthCheck Dict = property $ do
    (pk, _) <- genKeyPair @c
    address <- forAll $ genAddress pk
    let len = BS.length $ serializeAddress address
    annotateShow len
    assert (len > 0 && len <= 31)

{------------------------------------------------------------------------------
  Generators and other mythological creatures
------------------------------------------------------------------------------}

genAddress :: PublicKey c -> Gen (Address c)
genAddress pk = (`fromPublicKey` pk) <$> Gen.enumBounded
