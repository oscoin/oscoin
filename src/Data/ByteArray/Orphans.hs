{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ByteArray.Orphans where

import           Prelude hiding (length)

import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import qualified Codec.Serialise as CBOR

import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Radicle.Extended as Rad

instance ByteArrayAccess Rad.Value where
    length           = fromIntegral . LBS.length . CBOR.serialise
    withByteArray ba = withByteArray (LBS.toStrict $ CBOR.serialise ba)

instance ByteArrayAccess Text where
    length           = length . encodeUtf8
    withByteArray ba = withByteArray (encodeUtf8 ba)
