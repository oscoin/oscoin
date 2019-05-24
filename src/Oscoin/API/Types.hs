{-# LANGUAGE UndecidableInstances #-}
module Oscoin.API.Types
    ( Result(..)
    , TxLookupResponse(..)
    , isOk
    , isErr
    , resultToEither
    , TxSubmitResponse(..)
    ) where

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash (HasHashing, Hash, Hashed)
import           Oscoin.Data.Tx.Abstract
import           Oscoin.Prelude

import qualified Codec.Serialise as Serial
import           Numeric.Natural

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

-- | Response type of a transaction lookup API operation.
data TxLookupResponse c tx = TxLookupResponse
    { txHash          :: Hashed c tx
    -- ^ Hash of the transaction.
    , txBlockHash     :: Maybe (BlockHash c)
    -- ^ @BlockHash@ of the 'Block' in which the transaction was included.
    , txOutput        :: Maybe (TxOutput c tx)
    -- ^ Output of the transaction if it was evaluated. If the
    -- evaluation was successful the transaction is included in the
    -- block 'txBlockHash'.
    , txConfirmations :: Natural
    -- ^ Block depth of the 'Block' in which the transaction was included,
    -- which is the number of blocks from the tip up until, and including,
    -- the 'Block' referenced by 'txBlockHash'.
    , txPayload       :: tx
    -- ^ The transaction itself.
    } deriving (Generic)

deriving instance ( HasHashing c
                  , Show (Hash c)
                  , Show tx
                  , Show (TxOutput c tx)
                  ) => Show (TxLookupResponse c tx)
deriving instance ( Eq (Hash c)
                  , Eq tx
                  , Eq (TxOutput c tx)
                  ) => Eq (TxLookupResponse c tx)

instance ( Serial.Serialise (Hash c)
         , Serial.Serialise tx
         , Serial.Serialise (TxOutput c tx)
         ) => Serial.Serialise (TxLookupResponse c tx)

-- | A transaction receipt. Contains the hashed transaction.
newtype TxSubmitResponse c tx = TxSubmitResponse (Hashed c tx)


deriving instance Show (Hash c) => Show (TxSubmitResponse c tx)
deriving instance Eq (Hash c)   => Eq (TxSubmitResponse c tx)

deriving instance Serial.Serialise (Hash c) => Serial.Serialise (TxSubmitResponse c tx)
