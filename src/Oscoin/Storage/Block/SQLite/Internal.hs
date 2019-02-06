{-# LANGUAGE QuasiQuotes #-}
-- | SQLite block storage backend.
--
-- Nb. This module relies on the following invariant which is checked by
-- "storeBlock'": the timestamp of a 'Block' must always be greater than
-- the timestamp of its parent.
module Oscoin.Storage.Block.SQLite.Internal
    ( Handle(..)
    , open
    , close
    , initialize
    , isStored
    , isConflicting
    , storeOrphan
    , getGenesisBlock
    , getTip
    , getBlockTxs
    , getBlocks
    , getChainScore
    , getChainSuffixScore
    , getChainHashes
    , getOrphans
    , rollbackBlocks
    , storeBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Validate, validateBlockchain)
import           Oscoin.Crypto.Blockchain
                 (Depth, blocks, showChainDigest, unsafeToBlockchain)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Score
                 , blockHash
                 , isGenesisBlock
                 , mkBlock
                 , parentHash
                 )
import           Oscoin.Storage.Block.Orphanage (ChainCandidate, Orphanage)
import qualified Oscoin.Storage.Block.Orphanage as O
import           Oscoin.Time (Timestamp)

import           Paths_oscoin

import qualified Data.Map.Strict as Map

import           Database.SQLite.Simple as Sql ((:.)(..), Connection, Only(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow (FromRow)
import           Database.SQLite.Simple.Orphans ()
import           Database.SQLite.Simple.QQ
import           Database.SQLite.Simple.ToField (ToField)
import           Database.SQLite.Simple.ToRow (ToRow)
import qualified Database.SQLite3 as Sql3

import           Codec.Serialise
import           Control.Concurrent.STM
import qualified Data.Set as Set
import           GHC.Exts (IsList(fromList))

-- | A handle to an on-disk block store.
data Handle tx s = Handle
    { hConn    :: Connection               -- ^ Connection to on-disk storage for non-orphan blocks.
    , hOrphans :: TVar (Orphanage tx s)    -- ^ In-memory storage for orphans.
    , hScoreFn :: Block tx s -> Score      -- ^ Block scoring function.
    , hValidFn :: Validate tx s            -- ^ Block validation function.
    }

-- | Opens a connection to the SQLite DB.
open :: (Ord s, Ord tx)
     => String
     -> (Block tx s -> Score)
     -> Validate tx s
     -> IO (Handle tx s)
open path score validate =
    Handle <$> (Sql.open path >>= setupConnection)
           <*> newTVarIO (O.emptyOrphanage score)
           <*> pure score
           <*> pure validate
  where
    setupConnection conn = do
        enableForeignKeys conn
        pure conn
    enableForeignKeys conn =
        Sql3.exec (Sql.connectionHandle conn) "PRAGMA foreign_keys = ON;"

-- | Close a connection to the block store.
close :: Handle tx s -> IO ()
close Handle{..} = Sql.close hConn

-- | Initialize the block store with a genesis block. This can safely be run
-- multiple times.
initialize :: (ToRow tx, ToField s) => Block tx s -> Handle tx s -> IO (Handle tx s)
initialize gen h@Handle{hConn} =
    getDataFileName "data/blockstore.sql" >>=
        readFile >>=
        Sql3.exec (Sql.connectionHandle hConn) >>
            storeBlock' h gen >>
                pure h

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

-- | Store a block, along with its transactions in the block store.
storeBlock
    :: forall tx s.
        ( ToField s
        , FromRow tx
        , ToRow tx
        , Ord tx
        , Serialise s
        , FromField s
        , Ord s
        , Show s
        , Show tx
        ) => Handle tx s -> Block tx s -> IO ()
storeBlock h@Handle{..} blk = do
    let prevHash = parentHash blk
    orphans <- atomically (readTVar hOrphans)
    Sql.withTransaction hConn $ do
        blockExists       <- isStored hConn $ blockHash blk
        parentOnMainChain <- isStored hConn prevHash
        blockConflicts    <- isConflicting hConn blk
        currentTip        <- getTip h
        putStrLn ("\nblock: "  ++ show (blockHash blk))
        putStrLn ("\ncurrent_tip: "  ++ show (blockHash currentTip))
        putStrLn ("\nparent: " ++ show prevHash)
        if not blockExists && extendsTip currentTip blk && not blockConflicts
           then storeBlock' h blk
           else do
               let o' = O.insertOrphan blk orphans
               if parentOnMainChain
                  then case O.selectBestCandidate prevHash o' of
                           Nothing -> atomically $ writeTVar hOrphans o'
                           Just bc -> do
                               -- Compare the orphan best chain with the chain suffix
                               -- currently adopted
                               chainSuffix <- getChainSuffix hConn prevHash
                               when (O.fromChainSuffix hScoreFn chainSuffix < bc) $ do
                                   putStrLn $ "\nsuffix: " ++ toS (showChainDigest $ unsafeToBlockchain chainSuffix)
                                   rollbackBlocks hConn prevHash
                                   putStrLn $ "\ncandidate_raw: " ++ show (map blockHash $ O.toBlocksOldestFirst o' bc)
                                   putStrLn $ "\ncandidate: " ++ toS (showChainDigest $ unsafeToBlockchain (reverse $ O.toBlocksOldestFirst o' bc))
                                   storeBlocksOldestFirst h (O.toBlocksOldestFirst o' bc)
                               -- Either ways, we need to prune the chain which didn't won.
                               atomically $ writeTVar hOrphans (O.pruneOrphanage prevHash bc o')
                  else atomically $ writeTVar hOrphans o'
  where
    extendsTip :: Block tx s -> Block tx s -> Bool
    extendsTip currentTip blk = blockHash currentTip == (parentHash blk)

-- | Store an orphan in memory.
-- FIXME(adn) This is a bit of an abstraction leak, remove this function in
-- the future, or at least do not export it.
storeOrphan :: (Ord tx, Ord s) => Handle tx s -> Block tx s -> IO ()
storeOrphan Handle{..} = atomically . modifyTVar hOrphans . O.insertOrphan

getGenesisBlock :: (Serialise s, FromField s, FromRow tx) => Connection -> IO (Block tx s)
getGenesisBlock conn = do
    Only bHash :. bHeader <-
        headDef (panic "No genesis block!") <$> Sql.query_ conn
            [sql|   SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                      FROM blocks
                     WHERE parenthash IS NULL |]
    bTxs <- getBlockTxs conn bHash
    pure $ mkBlock bHeader bTxs

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

-- | Get the chain starting from a given hash up to the tip
getChainSuffix :: (FromRow tx, Serialise s, FromField s) => Connection -> BlockHash -> IO [Block tx s]
getChainSuffix conn hsh = do
    result <- lookupBlockTimestamp conn hsh
    case result of
        Nothing -> pure []
        Just t -> do
            rows :: [Only BlockHash :. BlockHeader s] <- Sql.query conn
                [sql|  SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                         FROM blocks WHERE timestamp > ?
                     ORDER BY timestamp DESC
                        |] (Only t)
            for rows $ \(Only h :. bh) ->
                mkBlock bh <$> getBlockTxs conn h

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

getOrphans :: Handle tx s -> IO (Set BlockHash)
getOrphans Handle{..} =
    O.getOrphans <$> readTVarIO hOrphans

-- | Rollback the blocks from the given 'BlockHash' (excluded) up to the tip.
rollbackBlocks :: Connection -> BlockHash -> IO ()
rollbackBlocks conn hsh = do
    result <- lookupBlockTimestamp conn hsh
    case result of
        Just t ->
            Sql.execute conn
                [sql| DELETE FROM blocks WHERE timestamp > ? |] (Only t)
        Nothing ->
            pure ()

-- | When given a list of blocks ordered by /oldest first/, it inserts them
-- into the store.
storeBlocksOldestFirst :: (ToRow tx, ToField s) => Handle tx s -> [Block tx s] -> IO ()
storeBlocksOldestFirst h = traverse_ (storeBlock' h)

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

getBlocks :: (Serialise s, FromField s, FromRow tx) => Handle tx s -> Depth -> IO [Block tx s]
getBlocks Handle{hConn} (fromIntegral -> depth :: Integer) = do
    rows :: [Only BlockHash :. BlockHeader s] <- Sql.query hConn
        [sql|  SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                 FROM blocks
             ORDER BY timestamp DESC
                LIMIT ? |] (Only depth)

    for rows $ \(Only h :. bh) ->
        mkBlock bh <$> getBlockTxs hConn h

getTip :: (Serialise s, FromField s, FromRow tx) => Handle tx s -> IO (Block tx s)
getTip h =
    headDef (panic "No blocks in storage!") <$> getBlocks h 1
