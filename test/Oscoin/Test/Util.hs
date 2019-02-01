{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Util where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Hash as Crypto

import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Formatting as F

-- | A typeclass for things that can be printed in a condensed way.
class Condensed a where
    condensed :: a -> Text

instance Condensed () where
    condensed () = "()"

instance Condensed a => Condensed (Set a) where
    condensed s = "[" <> T.intercalate "," (map condensed (Set.toList s)) <> "]"

instance Condensed BlockHash where
    condensed h = F.sformat F.string (C8.unpack . Crypto.shortHash $ h)

instance Condensed (Block tx s) where
    condensed = showBlockDigest
