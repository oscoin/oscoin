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
    , deleteOrphans
    , longestOrphanChain
    , getGenesisBlock
    , getBlockTxs
    , getChainScore
    , getChainSuffixScore
    , getChainHashes
    , getOrphans
    , revertBlocks
    , storeBlockchain'
    , storeBlock
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
                 , mkBlock
                 )
import           Oscoin.Time (Timestamp)

import           Paths_oscoin

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
    , hOrphans :: TVar (Set (Block tx s))  -- ^ In-memory storage for orphans.
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
           <*> newTVarIO mempty
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
storeBlock :: forall tx s. (ToField s, ToRow tx, Ord tx, Ord s) => Handle tx s -> Block tx s -> IO ()
storeBlock h@Handle{..} blk =
    Sql.withTransaction hConn $ do
        blockExists       <- isStored hConn $ blockHash blk
        blockParentExists <- isStored hConn $ blockPrevHash $ blockHeader blk
        blockConflicts    <- isConflicting hConn blk

        unless blockExists $
            if blockParentExists && not blockConflicts
            then
                storeBlock' h blk
            else do
                storeOrphan h blk
                findBestChain
  where
    chainScore score =
        sum . map score

    -- Look through the orphans for any chain that is higher scoring than the
    -- main chain, and replace the low scoring blocks on the main chain with
    -- the higher scoring ones.
    --
    -- Nb. This has shortcomings, namely that only the best chain from the
    -- point of view of the orphan set is compared with the main chain. It
    -- may be that a worse-scoring orphan chain is able to replace the suffix
    -- of the main chain simply by virtue of having a more recent parent,
    -- and therefore requiring a lower score to outperform the main chain suffix.
    --
    -- To solve this, we either need to check all orphan chains above a certain
    -- score against the main chain, or evict old orphan chains that haven't
    -- won against the main chain.
    findBestChain = do
        orphanChain <- longestOrphanChain h

        case lastMay (toList orphanChain) of
            Just b | parentHash <- blockPrevHash (blockHeader b) ->
                whenM (isStored hConn parentHash) $ do
                    suffixScore <- getChainSuffixScore hConn parentHash

                    let orphanScore = chainScore hScoreFn orphanChain
                     in when (orphanScore > suffixScore) $ do
                        revertBlocks hConn parentHash
                        storeBlockchain' h orphanChain
                        deleteOrphans h orphanChain
            _ ->
                pass


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
    Set.map blockHash <$> readTVarIO hOrphans

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
