{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Store and retrieve transaction.
--
-- The store can store a transaction type @tx@ with a crypto @c@ if
-- they satisfy the @'StorableTx' c tx@ constraint. This requires @tx@
-- to implement 'IsTxRow'. We provide such an instance for 'Tx'.
module Oscoin.Storage.Block.SQLite.Transaction
    ( StorableTx
    , IsTxRow(..)
    , TxRow(..)
    , TxRowDecodeError
    , getTx
    , getBlockTxs
    , storeTxs
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash (Hash, Hashable, hash)
import           Oscoin.Crypto.PubKey
import           Oscoin.Data.Tx

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS

import           Database.SQLite.Simple as Sql (Connection, Only(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromRow (FromRow, field)
import           Database.SQLite.Simple.Orphans ()
import           Database.SQLite.Simple.QQ
import           Database.SQLite.Simple.ToField (ToField, toField)


data TxRow = TxRow
    { txRowMessage :: ByteString
    , txRowAuthor  :: ByteString
    , txRowChainId :: Word16
    , txRowNonce   :: Word32
    , txRowContext :: ByteString
    }

instance FromRow TxRow where
    fromRow = do
        txRowMessage <- field
        txRowAuthor <- field
        txRowChainId <- field
        txRowNonce <- field
        txRowContext <- field
        pure $ TxRow{..}


newtype TxRowDecodeError = TxRowDecodeError String
    deriving (Show, Eq)

instance Exception TxRowDecodeError where
    displayException (TxRowDecodeError msg) =
        "Failed to decode transaction row: " <> msg



-- | Contraints that need to be satisfied for storing and retriving
-- transactions. @c@ is the crypto and @tx@ the transaction data type.
-- This implies the @'IsTxRow' tx@ constraint.
type StorableTx c tx = (Hashable c tx, ToField (Hash c), Serialise (Hash c), IsTxRow tx)

class IsTxRow a where
    toTxRow :: a -> TxRow
    fromTxRow :: TxRow -> Either TxRowDecodeError a

instance
    ( Serialise (Hash c)
    , Serialise (PublicKey c)
    , Serialise (Signature c)
    , Serialise msg
    ) => IsTxRow (Tx c msg) where
    toTxRow Tx{..} = TxRow
        { txRowContext = LBS.toStrict $ serialise txContext
        , txRowMessage = LBS.toStrict $ serialise txMessage
        , txRowAuthor = LBS.toStrict $ serialise txPubKey
        , txRowChainId = txChainId
        , txRowNonce = txNonce
        }

    fromTxRow TxRow{..} = first toDecodeError $ do
        txMessage <- deserialiseOrFail $ LBS.fromStrict txRowMessage
        txPubKey <- deserialiseOrFail $ LBS.fromStrict txRowAuthor
        txContext <- deserialiseOrFail $ LBS.fromStrict txRowContext
        pure $ Tx
            { txMessage
            , txPubKey
            , txChainId = txRowChainId
            , txNonce = txRowNonce
            , txContext
            }
      where
        toDecodeError :: DeserialiseFailure -> TxRowDecodeError
        toDecodeError = TxRowDecodeError . displayException

getTx :: forall c tx. (StorableTx c tx) => Sql.Connection -> Hash c -> IO (Maybe tx)
getTx conn hash' = do
    rows <- Sql.query conn
        [sql| SELECT message, author, chainid, nonce, context
                FROM transactions
               WHERE hash = ? |] (Only $ serialise hash')
    forM (listToMaybe rows) fromTxRowThrow


storeTxs :: forall c tx. (StorableTx c tx) => Sql.Connection -> Hash c -> [tx] -> IO ()
storeTxs conn blockHash txs =
    -- Nb. To relate transactions with blocks, we store an extra block hash field
    -- for each row in the transactions table.
    Sql.executeMany conn
        [sql| INSERT INTO transactions  (message, author, chainid, nonce, context, hash, blockhash)
              VALUES                    (?, ?, ?, ?, ?, ?, ?) |] (map toTxRowValues txs)
  where
    toTxRowValues tx =
        let TxRow {..} = toTxRow  tx
        in
            [ toField txRowMessage
            , toField txRowAuthor
            , toField txRowChainId
            , toField txRowNonce
            , toField txRowContext
            , toField $ serialise $ hash @c tx
            , toField blockHash
            ]

-- | Get the transactions belonging to a block.
getBlockTxs :: (StorableTx c tx) => Connection -> BlockHash c -> IO [tx]
getBlockTxs conn h = do
    txRows <- Sql.query conn
        [sql| SELECT message, author, chainid, nonce, context
                FROM transactions
               WHERE blockhash = ? |] (Only h)
    traverse fromTxRowThrow txRows

fromTxRowThrow :: (IsTxRow tx) => TxRow -> IO tx
fromTxRowThrow txRow = case fromTxRow txRow of
    Left err -> throw err
    Right tx -> pure tx
