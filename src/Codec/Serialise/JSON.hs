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

import           Codec.Serialise (Serialise, serialise)
import           Control.Monad.Fail (fail)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Lazy as LBS

serialiseToJSON :: (Serialise a) => a -> Aeson.Value
serialiseToJSON = Aeson.toJSON . BaseN.encodeBase64 . LBS.toStrict . serialise

deserialiseParseJSON :: (Serialise a) => Aeson.Value -> Aeson.Parser a
deserialiseParseJSON = Aeson.withText "deserialiseParseJSON" $ \t ->
    either (fail . show) pure $
        BaseN.deserialiseAtBase BaseN.Base64 (encodeUtf8 t)
