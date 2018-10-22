{-# LANGUAGE QuasiQuotes #-}

-- | Disk-backed block storage using SQLite.
module Oscoin.Storage.Block.SQLite
    ( Handle
    , fromHandle
    , withBlockStore
    , open
    , close
    , initialize
    , storeBlock
    , lookupBlock
    , lookupTx
    , getGenesisBlock
    , orphans
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
                 ( Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Orphan
                 , blockHash
                 , mkBlock
                 )
import qualified Oscoin.Crypto.Hash as Crypto

import           Database.SQLite.Simple (Connection, Only(..), (:.)(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromRow (FromRow)
import           Database.SQLite.Simple.ToRow (ToRow)
import           Database.SQLite.Simple.Orphans ()
import           Database.SQLite.Simple.QQ
import qualified Database.SQLite3 as Sql3

import qualified Data.Set as Set

-- | A handle to an on-disk block store.
newtype Handle tx s = Handle { fromHandle :: Connection }

-- | Open a connection to the block store.
open :: String -> IO (Handle tx s)
open path = Handle <$> Sql.open path

-- | Close a connection to the block store.
close :: Handle tx s -> IO ()
close (Handle conn) = Sql.close conn

withBlockStore :: String -> (Handle tx s -> IO b) -> IO b
withBlockStore path = bracket (open path) close

-- | Initialize the block store with a genesis block. This can safely be run
-- multiple times.
initialize :: ToRow tx => Block tx s -> Handle tx s -> IO (Handle tx s)
initialize gen h@(Handle conn) =
    readFile "data/blockstore.sql" >>=
        Sql3.exec (Sql.connectionHandle conn) >>
            storeBlock_ conn gen >>
                pure h

-- | Store a block, along with its transactions in the block store.
storeBlock :: forall tx s. ToRow tx => Handle tx s -> Block tx (Orphan s) -> IO ()
storeBlock (Handle conn) blk@Block{blockHeader = BlockHeader{..}} =
    Sql.withTransaction conn $ storeBlock_ conn blk

lookupBlock :: forall tx s. FromRow tx => Handle tx s -> BlockHash -> IO (Maybe (Block tx ()))
lookupBlock (Handle conn) h = Sql.withTransaction conn $ do
    result :: Maybe (BlockHeader ()) <- listToMaybe <$> Sql.query conn
        [sql| SELECT parenthash, datahash, timestamp, difficulty, nonce
                FROM blocks
               WHERE hash = ? |] (Only h)

    txs :: [tx] <- getBlockTxs conn h

    for result $ \bh ->
        pure $ mkBlock bh txs

lookupTx :: FromRow tx => Handle tx s -> Crypto.Hashed tx -> IO (Maybe tx)
lookupTx (Handle conn) h =
    listToMaybe <$> Sql.query conn
        [sql| SELECT message, author, chainid, nonce, context
                FROM transactions
               WHERE hash = ? |] (Only h)

getGenesisBlock :: FromRow tx => Handle tx s -> IO (Block tx ())
getGenesisBlock (Handle conn) = do
    Only bHash :. bHeader <-
        headDef (panic "No genesis block!") <$> Sql.query_ conn
            [sql|   SELECT hash, parenthash, datahash, timestamp, difficulty, nonce
                      FROM blocks
                  ORDER BY timestamp
                     LIMIT 1 |]
    bTxs <- getBlockTxs conn bHash
    pure $ mkBlock bHeader bTxs

orphans :: Handle tx s -> IO (Set BlockHash)
orphans (Handle conn) =
    Set.fromList . map fromOnly <$> Sql.query_ conn
        [sql| SELECT hash
                FROM blocks
               WHERE parenthash NOT IN (SELECT hash FROM blocks) |]

-- Internal --------------------------------------------------------------------

getBlockTxs :: FromRow tx => Connection -> BlockHash -> IO [tx]
getBlockTxs conn h =
    Sql.query conn
        [sql| SELECT message, author, chainid, nonce, context
                FROM transactions
               WHERE blockhash = ? |] (Only h)

-- | Store a block in the block store.
--
-- Must be called from within a transaction. Does not check whether the parent
-- reference is valid.
storeBlock_ :: ToRow tx => Connection -> Block tx s -> IO ()
storeBlock_ conn blk@Block{blockHeader = BlockHeader{..}, blockData} = do
    -- Nb. To relate transactions with blocks, we store an extra block hash field
    -- for each row in the transactions table.
    Sql.execute conn
        [sql| INSERT INTO blocks  (hash, timestamp, nonce, parenthash, datahash, difficulty)
              VALUES              (?, ?, ?, ?, ?, ?) |] row
    Sql.executeMany conn
        [sql| INSERT INTO transactions  (hash, message, author, chainid, nonce, context, blockhash)
              VALUES                    (?, ?, ?, ?, ?, ?, ?) |] txs
  where
    row = ( blockHash blk
          , blockTimestamp
          , blockNonce
          , blockPrevHash
          , blockDataHash
          , blockDifficulty
          )
    txs = [tx :. Only (blockHash blk) | tx <- toList blockData]
