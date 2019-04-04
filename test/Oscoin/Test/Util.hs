{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Util where

import           Oscoin.Prelude

import           Oscoin.API.Types (RadTx)
import           Oscoin.Crypto
import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.PubKey.Internal (SK(..))
import           Oscoin.Crypto.PubKey.Mock
import           Oscoin.Crypto.PubKey.RealWorld

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

instance Condensed Char where
    condensed = show

instance Condensed a => Condensed (Set a) where
    condensed = condensed . Set.toList

instance Condensed a => Condensed (Seq a) where
    condensed = condensed . toList

instance Condensed a => Condensed (Maybe a) where
    condensed Nothing  = "Nothing"
    condensed (Just x) = condensed x

instance (Condensed a, Condensed b) => Condensed (Either a b) where
    condensed (Left e)  = "L(" <> condensed e <> ")"
    condensed (Right x) = "R(" <> condensed x <> ")"

instance (Condensed a, Condensed b) => Condensed (a, b) where
    condensed (a,b) = "(" <> condensed a <> "," <> condensed b <> ")"

instance (Condensed a, Condensed b, Condensed c) => Condensed (a, b, c) where
    condensed (a,b,c) = "(" <> condensed a <> "," <> condensed b <> "," <> condensed c <> ")"

instance Condensed a => Condensed [a] where
    condensed xs = "[" <> T.intercalate "," (map condensed xs) <> "]"

instance Crypto.HasHashing c => Condensed (BlockHash c) where
    condensed h = F.sformat F.string (C8.unpack . Crypto.compactHash $ h)

instance Crypto.HasHashing c => Condensed (Block c tx s) where
    condensed = showBlockDigest

instance Crypto.HasHashing c => Condensed (Blockchain c tx s) where
    condensed = showChainDigest

instance Crypto.HasHashing c => Condensed (Crypto.Hashed c ByteString) where
    condensed = decodeUtf8 . Crypto.compactHash . Crypto.fromHashed

instance Condensed (PublicKey Crypto) where
    condensed = show

-- NOTE(adn) It's a bit of a security hazard to implement a 'Show' instance
-- for a private key, as that might end up in public logs and other crazy things.
-- This is why we define (in the tests only) such 'Condensed' instance.
instance Condensed (PrivateKey Crypto) where
    condensed (PrivateKey (SK k)) = show k

instance Condensed (PublicKey MockCrypto) where
    condensed = show

instance Condensed (PrivateKey MockCrypto) where
    condensed (MockSK (SK k)) = show k

instance Show (Signature c) => Condensed (Signed c Text) where
    condensed = show

showOrphans
    :: Crypto.HasHashing c
    => ( Blockchain c (RadTx c) s
       , [(Blockchain c (RadTx c) s, Block c (RadTx c) (Sealed c s))]
       )
    -> String
showOrphans (initialChain, orphansWithLinks) =
    "chain: "      <> T.unpack (showChainDigest initialChain) <> "\n" <>
    "orphans:\n- " <> T.unpack showOrphanAndLinks
  where
      showOrphanAndLinks =
          T.intercalate "- " . map (\(c,l) -> showChainDigest c <> " link: " <> showBlockDigest l <> "\n")
                             $ orphansWithLinks

-- The Ed's Kmett one weird trick.

data Dict a where
    Dict :: a => Dict a
