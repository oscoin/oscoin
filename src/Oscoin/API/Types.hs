{-# LANGUAGE UndecidableInstances #-}
module Oscoin.API.Types
    ( RadTx
    , Result(..)
    , TxLookupResponse(..)
    , isOk
    , isErr
    , resultToEither
    , TxSubmitResponse(..)
    ) where

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Blockchain.Eval (EvalError)
import           Oscoin.Crypto.Hash (HasHashing, Hash, Hashed)
import           Oscoin.Crypto.PubKey (PublicKey, Signature)
import           Oscoin.Data.RadicleTx (RadTx)
import qualified Oscoin.Data.RadicleTx as RadicleTx
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
data TxLookupResponse c = TxLookupResponse
    { txHash          :: Hashed c (RadicleTx.RadTx c)
    -- ^ Hash of the transaction.
    , txBlockHash     :: Maybe (BlockHash c)
    -- ^ @BlockHash@ of the 'Block' in which the transaction was included.
    , txOutput        :: Maybe (Either EvalError RadicleTx.Output)
    -- ^ Output of the transaction if it was evaluated. If the
    -- evaluation was successful the transaction is included in the
    -- block 'txBlockHash'.
    , txConfirmations :: Natural
    -- ^ Block depth of the 'Block' in which the transaction was included,
    -- which is the number of blocks from the tip up until, and including,
    -- the 'Block' referenced by 'txBlockHash'.
    , txPayload       :: RadicleTx.RadTx c
    -- ^ The transaction itself.
    } deriving (Generic)

deriving instance (HasHashing c, Show (Hash c), Show (RadTx c)) => Show (TxLookupResponse c)
deriving instance (Eq (Hash c), Eq (RadTx c))                   => Eq (TxLookupResponse c)

instance ( Serial.Serialise (Hash c)
         , Serial.Serialise (PublicKey c)
         , Serial.Serialise (Signature c)
         ) => Serial.Serialise (TxLookupResponse c)

-- | A transaction receipt. Contains the hashed transaction.
newtype TxSubmitResponse c tx = TxSubmitResponse (Hashed c tx)


deriving instance Show (Hash c) => Show (TxSubmitResponse c tx)
deriving instance Eq (Hash c)   => Eq (TxSubmitResponse c tx)

deriving instance Serial.Serialise (Hash c) => Serial.Serialise (TxSubmitResponse c tx)
