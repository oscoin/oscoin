{-# LANGUAGE QuasiQuotes #-}
-- | SQLite block storage backend.
--
-- Nb. This module relies on the following invariant which is checked by
-- "storeBlock'": the timestamp of a 'Block' must always be greater than
-- the timestamp of its parent.
module Oscoin.Storage.Block.SQLite.Internal
    ( Handle(..)
    , isStored
    , isConflicting
    , storeOrphan
    , deleteOrphans
    , longestOrphanChain
    , getBlockTxs
    , getChainScore
    , getChainSuffixScore
    , getChainHashes
    , revertBlocks
    , storeBlockchain'
    , storeBlock'
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Validate, validateBlockchain)
import           Oscoin.Crypto.Blockchain (blocks)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Score
                 , blockHash
                 , isGenesisBlock
                 )
import           Oscoin.Time (Timestamp)

import           Database.SQLite.Simple ((:.)(..), Connection, Only(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow (FromRow)
import           Database.SQLite.Simple.Orphans ()
import           Database.SQLite.Simple.QQ
import           Database.SQLite.Simple.ToField (ToField)
import           Database.SQLite.Simple.ToRow (ToRow)

import           Control.Concurrent.STM
import qualified Data.Set as Set
import           GHC.Exts (IsList(fromList))

-- | A handle to an on-disk block store.
data Handle tx s = Handle
    { hConn    :: Connection               -- ^ Connection to on-disk storage for non-orphan blocks.
    , hOrphans :: TVar (Set (Block tx s))  -- ^ In-memory storage for orphans.
    , hScoreFn :: Block tx s -> Score      -- ^ Block scoring function.
    , hValidFn :: Validate tx s            -- ^ Block validation function.
    }

-- | Check whether a given block hash exists.
isStored :: Connection -> BlockHash -> IO Bool
isStored conn bh =
    headDef False . map fromOnly <$> Sql.query conn
        [sql| SELECT EXISTS (SELECT 1 FROM blocks WHERE hash = ?) |] (Only bh)

-- | Check if a given block is conflicting with the main chain. This means a
-- block already exists with the same parent.
isConflicting :: Connection -> Block tx s -> IO Bool
isConflicting conn Block{blockHeader} =
    headDef False . map fromOnly <$> Sql.query conn
        [sql| SELECT EXISTS (SELECT 1 FROM blocks WHERE parenthash = ?) |]
        (Only $ blockPrevHash blockHeader)

-- | Store an orphan in memory.
storeOrphan :: (Ord tx, Ord s) => Handle tx s -> Block tx s -> IO ()
storeOrphan Handle{..} =
    atomically . modifyTVar hOrphans . Set.insert

deleteOrphans :: (Ord tx, Ord s) => Handle tx s -> [Block tx s] -> IO ()
deleteOrphans Handle{..} =
    atomically . modifyTVar hOrphans . Set.difference . Set.fromList

-- | Finds the longest orphan chain according to the scoring function in `Handle`.
longestOrphanChain :: Handle tx s -> IO [Block tx s]
longestOrphanChain Handle{..} =
    -- Sorts all orphan blocks by timestamp, and checks all subsequences for
    -- validity. Then choses the subsequence with the highest score as the
    -- result.
      longest
    . filter (isRight . validateBlockchain hValidFn)
    . map fromList
    . tailDef []
    . subsequences
    . sortOn (Down . blockTimestamp . blockHeader)
    . Set.toList <$> readTVarIO hOrphans
  where
    score = sum . map hScoreFn . blocks

    longest [] = []
    longest xs = blocks $ maximumBy (comparing score) xs

-- | Get the transactions belonging to a block.
getBlockTxs :: FromRow tx => Connection -> BlockHash -> IO [tx]
getBlockTxs conn h =
    Sql.query conn
        [sql| SELECT message, author, chainid, nonce, context
                FROM transactions
               WHERE blockhash = ? |] (Only h)

-- | Get the total score of the chain.
getChainScore :: Connection -> IO Integer
getChainScore conn =
     queryOne_ conn [sql| SELECT SUM(score) FROM blocks |]

-- | Get the score of the chain starting from a given hash.
getChainSuffixScore :: Connection -> BlockHash -> IO Integer
getChainSuffixScore conn hsh = do
    result <- lookupBlockTimestamp conn hsh

    case result of
        Just t ->
            queryOne conn
                [sql| SELECT SUM(score) FROM blocks WHERE timestamp > ? |] (Only t)
        Nothing ->
            pure 0

getChainHashes :: Connection -> IO [BlockHash]
getChainHashes conn =
    map fromOnly <$>
        Sql.query_ conn [sql| SELECT hash FROM blocks ORDER BY timestamp DESC |]

revertBlocks :: Connection -> BlockHash -> IO ()
revertBlocks conn hsh = do
    result <- lookupBlockTimestamp conn hsh
    case result of
        Just t ->
            Sql.execute conn
                [sql| DELETE FROM blocks WHERE timestamp > ? |] (Only t)
        Nothing ->
            pure ()

storeBlockchain' :: (ToRow tx, ToField s) => Handle tx s -> [Block tx s] -> IO ()
storeBlockchain' h = traverse_ (storeBlock' h) . reverse

lookupBlockTimestamp :: Connection -> BlockHash -> IO (Maybe Timestamp)
lookupBlockTimestamp conn hsh =
    map fromOnly . listToMaybe <$>
        Sql.query conn [sql| SELECT timestamp FROM blocks WHERE hash = ? |] (Only hsh)

-- | Store a block in the block store.
--
-- Must be called from within a transaction. Must include a valid parent reference.
storeBlock' :: (ToRow tx, ToField s) => Handle tx s -> Block tx s -> IO ()
storeBlock' Handle{..} blk@Block{blockHeader = BlockHeader{..}, blockData} = do
    parentTimestamp <- lookupBlockTimestamp hConn blockPrevHash

    -- Since we rely on the timestamp for block ordering, make sure this
    -- invariant is checked. Note that parent references are checked by the
    -- database.
    for_ parentTimestamp $ \t ->
        unless (blockTimestamp > t) $
            panic $ "Oscoin.Storage.Block.SQLite: "
                 <> "attempt to store block with invalid timestamp"

    -- Nb. To relate transactions with blocks, we store an extra block hash field
    -- for each row in the transactions table.
    Sql.execute hConn
        [sql| INSERT INTO blocks  (hash, timestamp, parenthash, datahash, statehash, difficulty, seal, score)
              VALUES              (?, ?, ?, ?, ?, ?, ?, ?) |] row
    Sql.executeMany hConn
        [sql| INSERT INTO transactions  (hash, message, author, chainid, nonce, context, blockhash)
              VALUES                    (?, ?, ?, ?, ?, ?, ?) |] txs
  where
    row = ( blockHash blk
          , blockTimestamp
          , blockParent
          , blockDataHash
          , blockStateHash
          , blockTargetDifficulty
          , blockSeal
          , hScoreFn blk
          )
    txs = [tx :. Only (blockHash blk) | tx <- toList blockData]

    blockParent =
        if isGenesisBlock blk
           then Nothing
           else Just blockPrevHash

-- | Query a single row and panic if it doesn't exist.
queryOne :: (HasCallStack, ToRow args, FromField b) => Connection -> Sql.Query -> args -> IO b
queryOne conn q args =
    headDef (panic "Oscoin.Storage.Block.SQLite.queryOne: table is empty")
        . map fromOnly <$> Sql.query conn q args

-- | Query a single row without parameters and panic if it doesn't exist.
queryOne_ :: (HasCallStack, FromField b) => Connection -> Sql.Query -> IO b
queryOne_ conn q =
    headDef (panic "Oscoin.Storage.Block.SQLite.queryOne_: table is empty")
        . map fromOnly <$> Sql.query_ conn q
