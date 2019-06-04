{-# LANGUAGE UndecidableInstances #-}
module Oscoin.API.Types
    ( Result(..)
    , isOk
    , isErr
    , resultToEither
    , TxSubmitResponse(..)
    ) where

import           Oscoin.Crypto.Hash (Hash, Hashed)
import           Oscoin.Prelude

import qualified Codec.Serialise as Serial

data Result a =
      Ok  a
    | Err Text
    deriving (Show, Eq, Functor, Generic)

instance Serial.Serialise a => Serial.Serialise (Result a)

isOk :: Result a -> Bool
isOk (Ok _) = True
isOk _      = False

isErr :: Result a -> Bool
isErr = not . isOk

resultToEither :: Result a -> Either Text a
resultToEither (Ok a ) = Right a
resultToEither (Err t) = Left t

-- | A transaction receipt. Contains the hashed transaction.
newtype TxSubmitResponse c tx = TxSubmitResponse (Hashed c tx)


deriving instance Show (Hash c) => Show (TxSubmitResponse c tx)
deriving instance Eq (Hash c)   => Eq (TxSubmitResponse c tx)

deriving instance Serial.Serialise (Hash c) => Serial.Serialise (TxSubmitResponse c tx)
