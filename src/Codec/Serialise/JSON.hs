-- | This module exports functions that allow one to define generic
-- `ToJSON` and `FromJSON` instances for values that are an instance of
-- `Serialise`.
--
-- @
--      data Foo
--
--      instance Serialise Foo
--
--      instance ToJSON where
--          toJSON = serialiseToJSON
--
--      instance FromJSON where
--          parseJSON = deserialiseParseJSON
-- @
--
-- This generic JSON serialisation scheme is unstructured. The value is
-- encoded into a CBOR bytestring which is encoded as base64 which can
-- be represented as a JSON string.
--
-- When possible we should provide custom JSON instances that are
-- easier to read and more structure.
module Codec.Serialise.JSON
    ( serialiseToJSON
    , deserialiseParseJSON
    ) where

import           Oscoin.Prelude

import           Codec.Serialise
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base64.Extended as Base64
import qualified Data.ByteString.Lazy as LBS

serialiseToJSON :: (Serialise a) => a -> Aeson.Value
serialiseToJSON = Aeson.toJSON . Base64.encodeLazy . serialise

deserialiseParseJSON :: (Serialise a) => Aeson.Value -> Aeson.Parser a
deserialiseParseJSON val = do
    base64 <- Aeson.parseJSON val
    bs <- case Base64.decodeOrFail base64 of
        Left err -> fail $ "Failed to decode base64 string: " <> err
        Right a -> pure a
    case deserialiseOrFail $ LBS.fromStrict bs of
        Left err -> fail $ show err
        Right a -> pure a
