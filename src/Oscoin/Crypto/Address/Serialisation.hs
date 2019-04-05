{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Oscoin.Crypto.Address.Serialisation
    ( DeserializeError(..)
    , serializeAddress
    , deserializeAddress
    ) where

import           Oscoin.Prelude

import           Oscoin.Configuration (Network(..))
import qualified Oscoin.Crypto.Address.Bech32 as Bech32
import           Oscoin.Crypto.Address.Internal
import           Oscoin.Crypto.Hash

import           Codec.CBOR.ByteArray (toSliced)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Monad.Except (Except, liftEither, throwError)
import           Control.Monad.Fail (fail)
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BL
import           Data.Tagged


{------------------------------------------------------------------------------
  Poor man's parsing primitives, to be kept here or moved into a separate
  package.
------------------------------------------------------------------------------}

data DeserializeError =
      NotEnoughInput  ByteString
      -- ^ The deserialisation failed as the decoder was expecting more
      -- input.
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

-- | A 'Decoder' is simply a 'StateT' transformer which carries around the
-- binary blob being decoded (and consumed) and using 'Except' at the bottom
-- of the stack, in order to be able to report precise errors in case the
-- decoding process fails.
type Decoder a = StateT ByteString (Except DeserializeError) a

-- | Consumes some input at the end of the blob. It doesn't consume anything
-- in case of failure.
consumeFromEnd :: Int -> Decoder ByteString
consumeFromEnd need = do
    x <- BS.unfoldrN need (map swap . BS.unsnoc) <$> get
    case x of
      (bs, Just rest) | BS.length bs == need -> do
          put rest
          pure (BS.reverse bs)
      (bs, _) -> lift . throwError $ NotEnoughInput bs

-- | Consumes some input at the start of the blob. It doesn't consume anything
-- in case of failure.
consume :: Int -> Decoder ByteString
consume need = do
    x <- BS.unfoldrN need BS.uncons <$> get
    case x of
      (bs, Just rest) | BS.length bs == need -> do
          put rest
          pure bs
      _ -> lift . throwError $ NotEnoughInput mempty

-- | Decodes a 'Word8', or fails otherwise.
decodeWord8 :: Decoder Word8
decodeWord8 = do
    x <- consume 1
    case BS.unpack x of
      [w] -> pure w
      _   -> throwError $ NotEnoughInput x

{------------------------------------------------------------------------------
    Computing the checksum
------------------------------------------------------------------------------}

-- | The 'Checksum' is computed using the same checksum schema in use
-- for <bech32 https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32>
-- addresses, which offers better performances and error-checking capabilities.
checksum
    :: ( CBOR.Serialise (ShortHash c)
       , ByteArrayAccess (ShortHash c)
       )
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

-- | Deserializes an 'Address'.
deserializeAddress
    :: ( CBOR.Serialise (ShortHash c)
       , ByteArrayAccess (ShortHash c)
       )
    => ByteString
    -> Either DeserializeError (Address c)
deserializeAddress blob = runExcept . flip evalStateT blob $ do
    crc <- Checksum <$> consumeFromEnd 6
    protectedData <- get

    unless (isValidChecksum protectedData crc) $
        throwError (InvalidChecksum crc)

    prefix <- AddressPrefix <$> deserializeProtocolVersion
                            <*> deserializeAddressType
                            <*> deserializeAddressFormat
    _reservedBytes <- consume 2
    Address <$> pure prefix <*> deserializeAddressPayload (addressFormat prefix)

-- | Deserializes the 'ProtocolVersion'.
deserializeProtocolVersion :: Decoder ProtocolVersion
deserializeProtocolVersion = do
    tag <- decodeWord8
    case tag of
      0x01 -> pure $ ProtocolVersion ProtocolVersion_V1
      _    -> throwError $ UnknownProtocolVersion tag

-- | Deserializes the 'AddressType'.
deserializeAddressType :: Decoder AddressType
deserializeAddressType = do
    tag <- decodeWord8
    case tag of
      0x01 -> pure $ AddressType Mainnet
      0x02 -> pure $ AddressType Testnet
      0x03 -> pure $ AddressType Devnet
      _    -> throwError $ UnknownAddressType tag

-- | Deserializes the 'AddressFormat'.
deserializeAddressFormat :: Decoder AddressFormat
deserializeAddressFormat = do
    tag <- decodeWord8
    case tag of
      0x01 -> pure $ AddressFormat AddressFormatTag_CBOR
      _    -> throwError $ UnknownAddressFormat tag

-- | Deserializes the 'AddressPayload' by piggybacking on CBOR.
deserializeAddressPayload
    :: ( ByteArrayAccess (ShortHash c)
       , CBOR.Serialise (ShortHash c)
       )
    => AddressFormat
    -> Decoder (AddressPayload c)
deserializeAddressPayload (AddressFormat tag) = do
    binaryBlob <- get
    -- Extension point to support multiple payloads in the future.
    case tag of
      AddressFormatTag_CBOR ->
          lift . liftEither $
              first DeserializePayloadError
                    (CBOR.deserialiseOrFail (toS binaryBlob))

{------------------------------------------------------------------------------
  Serialisation
------------------------------------------------------------------------------}

instance ( ByteArrayAccess (ShortHash c)
         , CBOR.Serialise (ShortHash c)
         ) => CBOR.Serialise (AddressPayload c) where
    encode (AddressPayload (Tagged p)) =
           CBOR.encodeListLen 2
        <> CBOR.encode (0 :: Word8)
        <> CBOR.encodeByteArray (toSliced p)
    decode = do
        CBOR.decodeListLenCanonicalOf 2
        tag <- CBOR.decodeWordCanonical
        case tag of
          0 -> AddressPayload . Tagged <$> CBOR.decodeByteArrayCanonical
          _ -> fail $ "Invalid tag when decoding an AddressPayload: " <> show tag

-- There is no 'Serialise' instance for 'Address', because in order to create
-- slightly shorter addresses we need to not encode the prefix via CBOR, so that
-- we can control the format of the z-base-32 output.

-- | Serializes an 'Address'.
serializeAddress
    :: ( CBOR.Serialise (ShortHash c)
       , ByteArrayAccess (ShortHash c)
       )
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
