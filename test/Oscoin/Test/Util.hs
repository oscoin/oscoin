{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Util where

import           Oscoin.Prelude

import           Oscoin.Crypto
import           Oscoin.Crypto.Address
import           Oscoin.Crypto.Address.Serialisation (DeserializeError)
import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.PubKey.Internal (SK(..))
import           Oscoin.Crypto.PubKey.Mock
import           Oscoin.Crypto.PubKey.RealWorld
import           Oscoin.Data.Ledger
import           Oscoin.Data.OscoinTx
import           Oscoin.Time.Chrono

import           Codec.Serialise (Serialise)
import qualified Crypto.Data.Auth.Tree as Tree
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Formatting as F

-- | A typeclass for things that can be printed in a condensed way.
class Condensed a where
    condensed :: a -> Text

instance Condensed () where
    condensed () = "()"

instance Condensed Bool where
    condensed = show

instance Condensed Integer where
    condensed = show

instance Condensed Int where
    condensed = show

instance Condensed Char where
    condensed = show

instance Condensed Text where
    condensed = toS

instance Condensed Word64 where
    condensed = show

instance Condensed Word32 where
    condensed = show

instance Condensed ByteString where
    condensed bs = BaseN.encodedText (BaseN.encodeBase16 bs)

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

instance Show (PublicKey c) => Condensed (PublicKey c) where
    condensed = show

instance ( Serialise (PublicKey c)
         ) => Condensed (Address c) where
    condensed = renderAddress

instance ( Show (PublicKey c)
         ) => Condensed (StateVal c) where
    condensed v =
        case v of
            AccountVal Account{..} ->
                "Account@" <> condensed accountId <> " $" <> condensed accountBalance
            ProjectVal project -> "Project@" <> condensed (projectId project)
            NatVal     natural -> condensed $ (fromIntegral natural :: Integer)

instance Condensed DeserializeError where
    condensed _ = "DeserializerError"

instance (Show (PublicKey c)) => Condensed (Project c) where
    condensed prj = "Project@" <> condensed (projectId prj)

instance (Show (PublicKey c)) => Condensed (Account c) where
    condensed acc = "Account@" <> condensed (accountId acc)

instance ( Show (PublicKey c)
         , Crypto.HasHashing c
         ) => Condensed (TxMessage c)
  where
    condensed (TxRegisterProject addr) =
        "TxRegisterProject " <> condensed addr
    condensed (TxUnregisterProject addr) =
        "TxUnregisterProject " <> condensed addr
    condensed (TxAuthorize addr user) =
        "TxAuthorize " <> condensed addr <> " " <> condensed user
    condensed (TxDeauthorize addr user) =
        "TxDeauthorize " <> condensed addr <> " " <> condensed user
    condensed (TxCheckpoint addr hsh _cs _ds) =
        "TxCheckpoint " <> condensed addr <> " " <> condensed hsh
    condensed (TxUpdateContract addr) =
        "TxUpdateContract " <> condensed addr
    condensed (TxTransfer addr bal) =
        "TxTransfer "
            <> condensed addr <> " "
            <> condensed bal

instance
    ( Show (PublicKey c)
    ) => Condensed (Tree.Tree StateKey (StateVal c))
  where
    condensed ws =
        let xs = [(k, v) | (k, v) <- Tree.toList ws]
         in condensed [(k, v) | (k, v) <- xs]

instance (Show (PublicKey c))
    => Condensed (TxError c)
  where
    condensed (ErrKeyNotFound k)         = "ErrKeyNotFound " <> condensed k
    condensed (ErrInvalidFee f)          = "ErrInvalidFee " <> condensed f
    condensed (ErrInsufficientBalance b) = "ErrInsufficientBalance " <> condensed b
    condensed (ErrNotAuthorized a)       = "ErrNotAuthorized " <> condensed a
    condensed (ErrInvalidNonce n)        = "ErrInvalidNonce " <> condensed n
    condensed (ErrInvalidTransfer b)     = "ErrInvalidTransfer " <> condensed b
    condensed (ErrTypeMismatch k)        = "ErrTypeMismatch " <> condensed k
    condensed (ErrProjectExists p)       = "ErrProjectExists " <> condensed p
    condensed (ErrInvalidTx t)           = "ErrInvalidTx " <> condensed t
    condensed (ErrOverflow a)            = "ErrOverflow " <> condensed a
    condensed (ErrHandlerFailed _)       = "ErrHandlerFailed"

instance Condensed TxMessageOutput where
    condensed _ = "TxMessageOutput"

instance ( Show (PublicKey c)
         , Crypto.HasHashing c
         ) => Condensed (Tx c)
  where
    condensed Tx'{..} = condensed txPayload

instance ( Show (PublicKey c)
         , Crypto.HasHashing c
         ) => Condensed (TxPayload c)
  where
    condensed TxPayload{..} = condensed txMessages

-- NOTE(adn) It's a bit of a security hazard to implement a 'Show' instance
-- for a private key, as that might end up in public logs and other crazy things.
-- This is why we define (in the tests only) such 'Condensed' instance.
instance Condensed (PrivateKey Crypto) where
    condensed (PrivateKey (SK k)) = show k

instance Condensed (PrivateKey MockCrypto) where
    condensed (MockSK (SK k)) = show k

instance Show (Signature c) => Condensed (Signed c Text) where
    condensed = show

instance Condensed (f a) => Condensed (OldestFirst f a) where
    condensed = condensed . toOldestFirst

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------
condensedS :: Condensed a => a -> String
condensedS = toS . condensed

-- The Ed's Kmett one weird trick.

data Dict a where
    Dict :: a => Dict a
