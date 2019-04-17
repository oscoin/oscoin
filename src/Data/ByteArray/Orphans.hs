{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ByteArray.Orphans where

import           Prelude hiding (length)

import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Data.ByteArray (ByteArrayAccess(..))

instance ByteArrayAccess Text where
    length           = length . encodeUtf8
    withByteArray ba = withByteArray (encodeUtf8 ba)
