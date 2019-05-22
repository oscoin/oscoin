{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Oscoin.Crypto.Address.Serialisation
    ( DeserializeError(..)
    , Decoder
    , serializeAddress
    , deserializeAddress

    -- * Internal & testing use only
    , deserializeAddressPrefix
    , isValidChecksum
    ) where

import           Oscoin.Prelude

import           Oscoin.Configuration (Network(..))
import qualified Oscoin.Crypto.Address.Bech32 as Bech32
import           Oscoin.Crypto.Address.Internal
import           Oscoin.Crypto.Hash (ShortHash)

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Monad.Except (liftEither, throwError)
import           Control.Monad.Fail (fail)
import           Data.Binary.Decoding
import qualified Data.ByteString.Builder as BL


{------------------------------------------------------------------------------
  Poor man's parsing primitives, to be kept here or moved into a separate
  package.
------------------------------------------------------------------------------}

data DeserializeError =
      DecodeError GetError
      -- ^ The deserialisation failed with a parsing error.
    | DeserializePayloadError CBOR.DeserialiseFailure
      -- ^ The deserialisation process failed while deserialising the
      -- 'AddressPayload'.
    | UnknownProtocolVersion Word8
      -- ^ When decoding the 'ProtocolVersion', we encountered an unknown tag.
    | UnknownAddressType Word8
      -- ^ When decoding the 'AddressType', we encountered an unknown tag.
    | UnknownAddressFormat Word8
      -- ^ When decoding the 'AddressFormat', we encountered an unknown tag.
    | InvalidChecksum Checksum
      -- ^ The computed checksum was invalid with respect to the one decoded.
      -- Returns the decoded checksum (i.e. the /actual/, not the /expected/.)
    | InvalidBase32zEncoding ByteString
      -- ^ The input bytestring wasn't a valid base32z-encoded one
    deriving (Eq, Show)

{------------------------------------------------------------------------------
    Computing the checksum
------------------------------------------------------------------------------}

-- | The 'Checksum' is computed using the same checksum schema in use
-- for <bech32 https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32>
-- addresses, which offers better performances and error-checking capabilities.
checksum
    :: CBOR.Serialise (ShortHash c)
    => Address c
    -> Checksum
checksum Address{addressPrefix, addressPayload} =
    let concatenated =
            serializeAddressPrefix addressPrefix
         <> toS (CBOR.serialise addressPayload)
    in Bech32.checksum concatenated

-- | Verifies the input 'Checksum'.
isValidChecksum :: ByteString -> Checksum -> Bool
isValidChecksum blob = Bech32.verifyChecksum blob

{------------------------------------------------------------------------------
  Deserialisation
------------------------------------------------------------------------------}

type Decoder a = ExceptT DeserializeError (State ByteString) a

-- | Deserializes an 'Address'.
deserializeAddress
    :: CBOR.Serialise (ShortHash c)
    => ByteString
    -> Either DeserializeError (Address c)
deserializeAddress blob = flip evalState blob . runExceptT $ do
    crc <- Checksum <$> withExceptT DecodeError (consumeFromEnd 6)
    protectedData <- get

    unless (isValidChecksum protectedData crc) $
        throwError (InvalidChecksum crc)

    prefix <- deserializeAddressPrefix
    _reservedBytes <- withExceptT DecodeError (consume 2)
    Address <$> pure prefix <*> deserializeAddressPayload (addressFormat prefix)

-- | Deserializes the 'AddressPrefix'.
deserializeAddressPrefix :: Decoder AddressPrefix
deserializeAddressPrefix =
    AddressPrefix <$> deserializeProtocolVersion
                  <*> deserializeAddressType
                  <*> deserializeAddressFormat

-- | Deserializes the 'ProtocolVersion'.
deserializeProtocolVersion :: Decoder ProtocolVersion
deserializeProtocolVersion = do
    tag <- withExceptT DecodeError getWord8
    case tag of
      0x01 -> pure $ ProtocolVersion ProtocolVersion_V1
      _    -> throwError $ UnknownProtocolVersion tag

-- | Deserializes the 'AddressType'.
deserializeAddressType :: Decoder AddressType
deserializeAddressType = do
    tag <- withExceptT DecodeError getWord8
    case tag of
      0x01 -> pure $ AddressType Mainnet
      0x02 -> pure $ AddressType Testnet
      0x03 -> pure $ AddressType Devnet
      _    -> throwError $ UnknownAddressType tag

-- | Deserializes the 'AddressFormat'.
deserializeAddressFormat :: Decoder AddressFormat
deserializeAddressFormat = do
    tag <- withExceptT DecodeError getWord8
    case tag of
      0x01 -> pure $ AddressFormat AddressFormatTag_CBOR
      _    -> throwError $ UnknownAddressFormat tag

-- | Deserializes the 'AddressPayload' by piggybacking on CBOR.
deserializeAddressPayload
    :: CBOR.Serialise (ShortHash c)
    => AddressFormat
    -> Decoder (AddressPayload c)
deserializeAddressPayload (AddressFormat tag) = do
    binaryBlob <- get
    -- Extension point to support multiple payloads in the future.
    case tag of
      AddressFormatTag_CBOR ->
          liftEither $
              first DeserializePayloadError
                    (CBOR.deserialiseOrFail (toS binaryBlob))

{------------------------------------------------------------------------------
  Serialisation
------------------------------------------------------------------------------}

instance CBOR.Serialise (ShortHash c) => CBOR.Serialise (AddressPayload c) where
    encode (AddressPayload_V0 sh) =
           CBOR.encodeListLen 2
        <> CBOR.encode (0 :: Word8)
        <> CBOR.encode sh
    decode = do
        CBOR.decodeListLenCanonicalOf 2
        tag <- CBOR.decodeWordCanonical
        case tag of
          0 -> AddressPayload_V0 <$> CBOR.decode
          _ -> fail $ "Invalid tag when decoding an AddressPayload: " <> show tag

-- There is no 'Serialise' instance for 'Address', because in order to create
-- slightly shorter addresses we need to not encode the prefix via CBOR, so that
-- we can control the format of the z-base-32 output.

-- | Serializes an 'Address'.
serializeAddress
    :: CBOR.Serialise (ShortHash c)
    => Address c
    -> ByteString
serializeAddress addr@Address{..} =
       serializeAddressPrefix addressPrefix
    <> toS (CBOR.serialise addressPayload)
    <> fromChecksum (checksum addr)

-- | Serializes an 'AddressPrefix'.
serializeAddressPrefix :: AddressPrefix -> ByteString
serializeAddressPrefix AddressPrefix{..} = toS . BL.toLazyByteString $
       serializeProtocolVersion protocolVersion
    <> serializeAddressType addressType
    <> serializeAddressFormat addressFormat
    <> serializeReservedBytes
  where
    serializeReservedBytes :: BL.Builder
    serializeReservedBytes = BL.word8 0x0 <> BL.word8 0x0

-- | Serializes the 'ProtocolVersion'.
serializeProtocolVersion :: ProtocolVersion -> BL.Builder
serializeProtocolVersion = \case
  ProtocolVersion ProtocolVersion_V1 -> BL.word8 0x01

-- | Serializes the 'AddressType'.
serializeAddressType :: AddressType -> BL.Builder
serializeAddressType = \case
  AddressType Mainnet -> BL.word8 0x01
  AddressType Testnet -> BL.word8 0x02
  AddressType Devnet  -> BL.word8 0x03

-- | Serializes the 'AddressFormat'.
serializeAddressFormat :: AddressFormat -> BL.Builder
serializeAddressFormat = \case
  AddressFormat AddressFormatTag_CBOR -> BL.word8 0x01
