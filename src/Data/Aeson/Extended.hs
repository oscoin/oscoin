{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Aeson.Extended where

import           Prelude

import qualified Data.ByteString.Base64.Extended as Base64
import           Data.ByteString.Base64.Extended (Base64(Base64))
import           Data.Aeson
import           Data.ByteString
import           Data.Text.Encoding (encodeUtf8)

instance ToJSON ByteString where
    toJSON = toJSON . Base64.encode

instance FromJSON ByteString where
    parseJSON = withText "ByteString" $
        pure . Base64.decode . (Base64 . encodeUtf8)
