{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Util where

import           Oscoin.Prelude

import           Oscoin.API.Types (RadTx)
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

instance Condensed Integer where
    condensed = show

instance Condensed Int where
    condensed = show

instance Condensed a => Condensed (Set a) where
    condensed = condensed . Set.toList

instance Condensed a => Condensed (Seq a) where
    condensed = condensed . toList

instance Condensed a => Condensed (Maybe a) where
    condensed Nothing  = "Nothing"
    condensed (Just x) = condensed x

instance Condensed a => Condensed [a] where
    condensed xs = "[" <> T.intercalate "," (map condensed xs) <> "]"

instance Condensed BlockHash where
    condensed h = F.sformat F.string (C8.unpack . Crypto.shortHash $ h)

instance Condensed (Block tx s) where
    condensed = showBlockDigest

showOrphans :: (Blockchain RadTx s, [(Blockchain RadTx s, Block RadTx s)])
            -> String
showOrphans (initialChain, orphansWithLinks) =
    "chain: "      <> T.unpack (showChainDigest initialChain) <> "\n" <>
    "orphans:\n- " <> T.unpack (showOrphanAndLinks)
  where
      showOrphanAndLinks =
          T.intercalate "- " . map (\(c,l) -> showChainDigest c <> " link: " <> showBlockDigest l <> "\n")
                             $ orphansWithLinks
