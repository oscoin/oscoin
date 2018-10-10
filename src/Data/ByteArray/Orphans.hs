{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ByteArray.Orphans where

import           Prelude ((.))

import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteArray as ByteArray
import           Data.Tagged (Tagged(..))

instance ByteArrayAccess b => ByteArrayAccess (Tagged s b) where
    length                    = ByteArray.length . unTagged
    withByteArray (Tagged ba) = withByteArray ba
