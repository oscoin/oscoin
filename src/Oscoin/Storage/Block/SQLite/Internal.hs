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

import           Oscoin.Consensus (Validate)
import           Oscoin.Crypto.Blockchain (Depth)
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
import           Oscoin.Storage.Block.Orphanage (Orphanage)
import qualified Oscoin.Storage.Block.Orphanage as O
import           Oscoin.Time (Timestamp)

import           Paths_oscoin

import qualified Data.List.NonEmpty as NonEmpty

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

-- | A handle to an on-disk block store.
data Handle tx s = Handle
    { hConn    :: Connection               -- ^ Connection to on-disk storage for non-orphan blocks.
    , hOrphans :: TVar (Orphanage tx s)    -- ^ In-memory storage for orphans.
    , hScoreFn :: Block tx s -> Score      -- ^ Block scoring function.
    , hValidFn :: Validate tx s            -- ^ Block validation function.
    }

-- | Opens a connection to the SQLite DB.
open :: String
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
-- Invariant: the input block must have been validate (cfr. 'applyBlock').
storeBlock
    :: forall tx s.
        ( ToField s
        , FromRow tx
        , ToRow tx
        , Ord tx
        , Serialise s
        , FromField s
        , Ord s
        ) => Handle tx s -> Block tx s -> IO ()
storeBlock h@Handle{..} blk =
    Sql.withTransaction hConn $ do
        genesisBlock       <- getGenesisBlock hConn
        blockExists        <- isStored hConn $ blockHash blk
        blockConflicts     <- isConflicting hConn blk
        currentTip         <- getTip h

        if not blockExists && extendsTip currentTip && not blockConflicts
            then storeBlock' h blk
            -- FIXME(adn) Sob, this is a side effect of the fact that storing
            -- the genesis as an orphan would insert it into the orphanage with
            -- its parent hash, which means that we won't be able to retrieve
            -- a potentiol candidate branching off from genesis in our 'selectBestChain'
            -- function (cfr. QuickCheck seed 554786). To avoid that, we avoid
            -- inserting the genesis block into the orphanage in the first place;
            -- After all, the genesis block should never be considered at orphan.
            -- However, this is the least satisfactory solution, as it means we
            -- need to hit the DB to grab the genesis /at every block insertion/.
            -- We have a couple of fixes here:
            -- 1. We cache the genesis block in memory;
            -- 2. We add a precondition that we should never insert genesis in
            --    the first place, so that this could would panic;
            -- 3. We clearly state this invariant and amend our QC generators so
            --    that we never generate chains starting from genesis.
            else unless (blk == genesisBlock) $ storeOrphan h blk

        -- Fork selection starts now.
        selectBestChain h
  where
    extendsTip :: Block tx s -> Bool
    extendsTip currentTip = blockHash currentTip == parentHash blk

selectBestChain
    :: forall tx s.
       ( Eq s
       , FromRow tx
       , Serialise s
       , FromField s
       , ToField s
       , ToRow tx
       )
    => Handle tx s -> IO ()
selectBestChain h@Handle{..} = do
    -- FIXME(adn) (Temporary hack) The idea here is to grab the last 2016
    -- blocks, which are considered the \"mutable\" part of the chain and
    -- compare each root hash to see if there is any fork originating from that.
    mutableBlockHashes <- reverse <$> getChainHashes hConn 2016

    orphans <- readTVarIO hOrphans

    case O.selectBestChain mutableBlockHashes orphans of
        Nothing -> pure ()
        Just (rootHash,bc) -> do
            -- Compare the orphan best chain with the chain suffix currently adopted
            chainSuffix <- getChainSuffix hConn rootHash

            -- Switch-to-better-chain condition: either the chain suffix is
            -- empty, which means we are extending directly the tip, or if we
            -- found a better candidate.
            let newChainFound = null chainSuffix
                             || O.fromChainSuffix hScoreFn (NonEmpty.fromList chainSuffix) < bc

            when newChainFound $
                switchToFork rootHash bc orphans chainSuffix

  where
    switchToFork rootHash fork orphans chainSuffix = do
        rollbackBlocks hConn (fromIntegral $ length chainSuffix)
        storeBlocksOldestFirst h (O.toBlocksOldestFirst orphans fork)
        -- Prune the chain which won, /as well as/ every dangling orphan
        -- originating from the suffix chain which has been replaced.
        atomically $ writeTVar hOrphans $
              foldl' (\acc blockInSuffix ->
                         O.pruneOrphanage (blockHash blockInSuffix)
                                          (O.fromChainSuffix hScoreFn (blockInSuffix NonEmpty.:| []))
                                          $ acc
                     ) (O.pruneOrphanage rootHash fork orphans) chainSuffix


-- | Store an orphan in memory.
storeOrphan :: Handle tx s -> Block tx s -> IO ()
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

getChainHashes :: Connection -> Depth -> IO [BlockHash]
getChainHashes conn (fromIntegral -> depth :: Integer) =
    map fromOnly <$>
      Sql.query conn [sql| SELECT hash FROM blocks ORDER BY timestamp DESC LIMIT ? |] (Only depth)

getOrphans :: Handle tx s -> IO (Set BlockHash)
getOrphans Handle{..} =
    O.getOrphans <$> readTVarIO hOrphans

-- | Rollback 'n' blocks.
rollbackBlocks :: Connection -> Depth -> IO ()
rollbackBlocks conn (fromIntegral -> depth :: Integer) =
    Sql.execute conn
        [sql| DELETE FROM blocks WHERE hash IN
              (SELECT hash from blocks ORDER BY timestamp DESC LIMIT ?)
        |] (Only depth)

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
