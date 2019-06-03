{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Store and retrieve transactions.
module Oscoin.Storage.Block.SQLite.Internal.Tx
    ( StorableTx
    , TxRow(..)
    , TxRowDecodeError
    , lookupTx
    , getBlockTxs
    , storeTxs
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash (Hash, Hashable, fromHashed, hash)
import           Oscoin.Crypto.Hash.RealWorld ()
import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.PubKey.RealWorld ()
import           Oscoin.Data.Ledger (AccountId, Balance, Nonce)
import           Oscoin.Data.OscoinTx

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS

import           Database.SQLite.Simple as Sql (Connection, Only(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow (FromRow, field)
import           Database.SQLite.Simple.Orphans ()
import           Database.SQLite.Simple.QQ
import           Database.SQLite.Simple.ToField (ToField, toField)

-- | Constraints required for a @Tx c@ to be storable in SQLite.
type StorableTx c =
    ( Serialise (Signature c)
    , Serialise (AccountId c)
    , Serialise (PublicKey c)
    , Serialise (Hash c)
    , FromField (Signature c)
    , FromField (Hash c)
    , ToField (Signature c)
    , ToField (Hash c)
    , Hashable c (Tx c)
    )

data TxRow c = TxRow
    { txRowHash      :: Hash c
    , txRowNetwork   :: Word8
    , txRowSignature :: Signature c
    , txRowMessages  :: ByteString
    , txRowNonce     :: Nonce
    , txRowFee       :: Balance
    , txRowBurn      :: Balance
    , txRowAuthor    :: PublicKey c
    }

instance (StorableTx c, Typeable c) => FromRow (TxRow c) where
    fromRow = do
        txRowHash      <- field
        txRowNetwork   <- field
        txRowSignature <- field
        txRowMessages  <- field
        txRowNonce     <- field
        txRowFee       <- field
        txRowBurn      <- field
        txRowAuthor    <- field

        pure TxRow{..}


newtype TxRowDecodeError = TxRowDecodeError String
    deriving (Show, Eq)

instance Exception TxRowDecodeError where
    displayException (TxRowDecodeError msg) =
        "Failed to decode transaction row: " <> msg

toTxRow ::
    ( StorableTx c
    ) => Tx c -> TxRow c
toTxRow tx@Tx'{..} = TxRow
    { txRowHash      = fromHashed $ hash tx
    , txRowNetwork   = fromIntegral $ fromEnum txNetwork
    , txRowSignature = txSignature
    , txRowMessages  = LBS.toStrict $ serialise (txMessages txPayload)
    , txRowNonce     = txNonce txPayload
    , txRowFee       = txFee txPayload
    , txRowBurn      = txBurn txPayload
    , txRowAuthor    = txAuthor txPayload
    }

fromTxRow :: StorableTx c => TxRow c -> Either TxRowDecodeError (Tx c)
fromTxRow TxRow{..} = first toDecodeError $ do
    txNetwork   <- pure $ toEnum (fromIntegral txRowNetwork)
    txSignature <- pure txRowSignature
    txMessages  <- deserialiseOrFail $ LBS.fromStrict txRowMessages
    txNonce     <- pure txRowNonce
    txFee       <- pure txRowFee
    txBurn      <- pure txRowBurn
    txAuthor    <- pure txRowAuthor

    pure Tx'
        { txPayload = TxPayload
            { txMessages
            , txNonce
            , txFee
            , txBurn
            , txAuthor
            }
        , txNetwork
        , txSignature
        }
  where
    toDecodeError :: DeserialiseFailure -> TxRowDecodeError
    toDecodeError = TxRowDecodeError . displayException

lookupTx :: forall c. (StorableTx c, Typeable c) => Sql.Connection -> Hash c -> IO (Maybe (Tx c))
lookupTx conn hsh = do
    rows <- Sql.query conn
        [sql| SELECT hash, network, signature, messages, nonce, fee, burn, author
                FROM transactions
               WHERE hash = ? |] (Only hsh)
    forM (listToMaybe rows) (fromTxRowThrow @c)


storeTxs :: forall c. (StorableTx c) => Sql.Connection -> Hash c -> [Tx c] -> IO ()
storeTxs conn blockHash txs =
    -- Nb. To relate transactions with blocks, we store an extra block hash field
    -- for each row in the transactions table.
    Sql.executeMany conn
        [sql| INSERT INTO transactions  (hash, network, signature, messages, nonce, fee, burn, author, blockhash)
              VALUES                    (?, ?, ?, ?, ?, ?, ?, ?, ?) |] (map toTxRowValues txs)
  where
    toTxRowValues tx =
        let TxRow{..} = toTxRow @c tx
        in
            [ toField txRowHash
            , toField txRowNetwork
            , toField txRowSignature
            , toField txRowMessages
            , toField txRowNonce
            , toField txRowFee
            , toField txRowBurn
            , toField txRowAuthor
            , toField blockHash
            ]

-- | Get the transactions belonging to a block.
getBlockTxs :: forall c. (StorableTx c, Typeable c) => Connection -> BlockHash c -> IO [Tx c]
getBlockTxs conn h = do
    txRows <- Sql.query conn
        [sql| SELECT hash, network, signature, messages, nonce, fee, burn, author
                FROM transactions
               WHERE blockhash = ? |] (Only h)
    traverse (fromTxRowThrow @c) txRows

fromTxRowThrow :: StorableTx c => TxRow c -> IO (Tx c)
fromTxRowThrow txRow = case fromTxRow txRow of
    Left err -> throw err
    Right tx -> pure tx
