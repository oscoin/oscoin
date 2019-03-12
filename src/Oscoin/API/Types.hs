{-# LANGUAGE UndecidableInstances #-}
module Oscoin.API.Types
    ( RadTx
    , Result(..)
    , TxLookupResponse(..)
    , isOk
    , isErr
    , resultToEither
    , TxSubmitResponse(..)
    , Key
    ) where

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Blockchain.Eval (EvalError)
import           Oscoin.Crypto.Hash (HasHashing, Hash, Hashable, Hashed)
import           Oscoin.Crypto.PubKey (PK, Signature)
import           Oscoin.Data.RadicleTx (RadTx)
import qualified Oscoin.Data.RadicleTx as RadicleTx
import           Oscoin.Node.Tree (Key)
import           Oscoin.Prelude
import qualified Radicle.Extended as Rad

import qualified Codec.Serialise as Serial
import           Data.Aeson
                 ( FromJSON
                 , ToJSON
                 , object
                 , parseJSON
                 , toJSON
                 , withObject
                 , (.:)
                 , (.=)
                 )
import           Lens.Micro
import           Numeric.Natural

data Result a =
      Ok  a
    | Err Text
    deriving (Show, Eq, Functor, Generic)

instance ToJSON a => ToJSON (Result a)
instance FromJSON a => FromJSON (Result a)
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

instance (ToJSON (RadTx c), ToJSON (Hash c)) => ToJSON (TxLookupResponse c) where
    toJSON TxLookupResponse{..} = object
        [ "txHash" .= toJSON txHash
        , "txBlockHash" .= toJSON txBlockHash
        , "txOutput" .= toJSON (map (second Rad.prettyValue) $ txOutput)
        , "txConfirmations" .= toJSON txConfirmations
        , "txPayload" .= toJSON txPayload
        ]

instance (FromJSON (RadTx c), FromJSON (Hash c)) => FromJSON (TxLookupResponse c) where
    parseJSON = withObject "TxLookupResponse" $ \o -> do
        txHash <- o .: "txHash"
        txBlockHash <- o .: "txBlockHash"
        txOutput <- o .: "txOutput" >>= traverseOf (_Just . _Right) Rad.parseFromJson
        txConfirmations <- o .: "txConfirmations"
        txPayload <- o .: "txPayload"
        pure $ TxLookupResponse {..}


instance ( Serial.Serialise (Hash c)
         , Serial.Serialise (PK c)
         , Serial.Serialise (Signature c)
         ) => Serial.Serialise (TxLookupResponse c)

-- | A transaction receipt. Contains the hashed transaction.
newtype TxSubmitResponse c tx = TxSubmitResponse (Hashed c tx)


deriving instance Show (Hash c) => Show (TxSubmitResponse c tx)
deriving instance Eq (Hash c)   => Eq (TxSubmitResponse c tx)

deriving instance Serial.Serialise (Hash c) => Serial.Serialise (TxSubmitResponse c tx)

instance (ToJSON (Hash c), Hashable c tx) => ToJSON (TxSubmitResponse c tx) where
    toJSON (TxSubmitResponse tx) =
        object [ "tx" .= tx ]

instance (FromJSON (Hash c), Hashable c tx) => FromJSON (TxSubmitResponse c tx) where
    parseJSON = withObject "TxSubmitResponse" $ \o ->
        TxSubmitResponse <$> o .: "tx"
