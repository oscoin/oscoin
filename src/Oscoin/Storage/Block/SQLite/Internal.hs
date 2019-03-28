{-# LANGUAGE QuasiQuotes #-}
-- | SQLite block storage backend.
--
-- Nb. This module relies on the following invariant which is checked by
-- "storeBlock'": the timestamp of a 'Block' must always be greater than
-- the timestamp of its parent.
module Oscoin.Storage.Block.SQLite.Internal
    ( Handle(..)
    , TxRow(..)
    , IsTxRow(..)
    , StorableTx
    , open
    , close
    , initialize
    , isStored
    , isConflicting
    , getGenesisBlock
    , getTip
    , getBlockTxs
    , getBlocks
    , getTx
    , getChainSuffix
    , getChainHashes
    , switchToFork
    , storeBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Depth)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Sealed
                 , blockHash
                 , isGenesisBlock
                 , mkBlock
                 , parentHash
                 )
import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Storage.Block.SQLite.Transaction
import           Oscoin.Time (Timestamp)
import           Oscoin.Time.Chrono (OldestFirst(..))
import           Oscoin.Time.Chrono as Chrono

import           Paths_oscoin

import           Database.SQLite.Simple as Sql ((:.)(..), Connection, Only(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.Orphans ()
import           Database.SQLite.Simple.QQ
import           Database.SQLite.Simple.ToField (ToField)
import qualified Database.SQLite3 as Sql3

import           Codec.Serialise
import           Oscoin.Storage.Block.Cache (BlockCache)
import qualified Oscoin.Storage.Block.Cache as BlockCache

-- | A handle to an on-disk block store.
data Handle c tx s = Handle
    { hConn       :: Connection                    -- ^ Connection to on-disk storage for non-orphan blocks.
    , hBlockCache :: BlockCache c tx (Sealed c s)  -- ^ In-memory cache for most recent blocks.
    }

-- | Opens a connection to the SQLite DB.
open
    :: String
    -> IO (Handle c tx s)
open path =
    Handle <$> (Sql.open path >>= setupConnection)
           <*> BlockCache.newBlockCache 2016 -- TODO(and) pass this parameter externally
  where
    setupConnection conn = do
        enableForeignKeys conn
        pure conn
    enableForeignKeys conn =
        Sql3.exec (Sql.connectionHandle conn) "PRAGMA foreign_keys = ON;"

-- | Close a connection to the block store.
close :: Handle c tx s -> IO ()
close Handle{..} = Sql.close hConn

-- | Initialize the block store with a genesis block. This can safely be run
-- multiple times.
initialize
    :: ( StorableTx c tx
       , ToField (Sealed c s)
       , Eq (Hash c)
       )
    => Block c tx (Sealed c s)
    -> Handle c tx s
    -> IO (Handle c tx s)
initialize gen h@Handle{hConn} =
    getDataFileName "data/blockstore.sql" >>=
        readFile >>= \schema -> do
          Sql3.exec (Sql.connectionHandle hConn) schema
          unlessM (isStored hConn (blockHash gen)) $
              storeBlock' h gen
          pure h

-- | Check whether a given block hash exists.
isStored
    :: ToField (Hash c)
    => Connection
    -> BlockHash c
    -> IO Bool
isStored conn bh =
    headDef False . map fromOnly <$> Sql.query conn
        [sql| SELECT EXISTS (SELECT 1 FROM blocks WHERE hash = ?) |] (Only bh)

-- | Check if a given block is conflicting with the main chain. This means a
-- block already exists with the same parent.
isConflicting
    :: ToField (Hash c)
    => Connection
    -> Block c tx s
    -> IO Bool
isConflicting conn block =
    headDef False . map fromOnly <$> Sql.query conn
        [sql| SELECT EXISTS (SELECT 1 FROM blocks WHERE parenthash = ?) |]
        (Only $ blockPrevHash $ blockHeader block)

-- | Store a block, along with its transactions in the block store.
-- Invariant: the input block must have passed \"basic\" validation.
-- (cfr. 'applyBlock').
storeBlock
    :: forall c tx s.
        ( ToField (Sealed c s)
        , StorableTx c tx
        , Serialise s
        , FromField (Sealed c s)
        , FromField (Hash c)
        , Ord (BlockHash c)
        ) => Handle c tx s
          -> Block c tx (Sealed c s)
          -> IO ()
storeBlock h@Handle{..} blk =
    Sql.withTransaction hConn $ do
        blockExists        <- isStored hConn $ blockHash blk
        blockConflicts     <- isConflicting hConn blk
        currentTip         <- getTip h

        if not blockExists && extendsTip currentTip && not blockConflicts
            then storeBlock' h blk
            else pure ()

  where
    extendsTip :: Block c tx (Sealed c s) -> Bool
    extendsTip currentTip = blockHash currentTip == parentHash blk


getGenesisBlock
    :: forall c s tx.
       ( Serialise s
       , FromField s
       , StorableTx c tx
       , FromField (Hash c)
       )
    => Connection
    -> IO (Block c tx s)
getGenesisBlock conn = do
    Only bHash :. bHeader <-
        headDef (panic "No genesis block!") <$> Sql.query_ conn
            [sql|   SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                      FROM blocks
                     WHERE parenthash IS NULL |]
    bTxs <- getBlockTxs @c conn bHash
    pure $ mkBlock bHeader bTxs

-- | Get the chain starting from a given hash up to the tip
getChainSuffix
    :: ( StorableTx c tx
       , Serialise s
       , FromField s
       , FromField (Hash c)
       )
    => Connection
    -> BlockHash c
    -> IO (NewestFirst [] (Block c tx s))
getChainSuffix conn hsh = do
    result <- lookupBlockTimestamp conn hsh
    case result of
        Nothing -> pure mempty
        Just t -> do
            rows :: [Only (BlockHash c) :. BlockHeader c s] <- Sql.query conn
                [sql|  SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                         FROM blocks WHERE timestamp > ?
                     ORDER BY timestamp DESC
                        |] (Only t)
            map NewestFirst <$> for rows $ \(Only h :. bh) ->
                mkBlock bh <$> getBlockTxs conn h

getChainHashes
    :: FromField (Hash c)
    => Connection
    -> Depth
    -> IO [BlockHash c]
getChainHashes conn (fromIntegral -> depth :: Integer) =
    map fromOnly <$>
      Sql.query conn [sql| SELECT hash FROM blocks ORDER BY timestamp DESC LIMIT ? |] (Only depth)

-- | Rollback 'n' blocks.
rollbackBlocks :: Handle c tx s -> Depth -> IO ()
rollbackBlocks Handle{..} (fromIntegral -> depth :: Integer) = do
    Sql.execute hConn
        [sql| DELETE FROM blocks WHERE hash IN
              (SELECT hash from blocks ORDER BY timestamp DESC LIMIT ?)
        |] (Only depth)
    BlockCache.invalidate hBlockCache

switchToFork
    :: ( StorableTx c tx
       , ToField s
       , Eq (Hash c)
       )
    => Handle c tx s
    -> Depth
    -> OldestFirst NonEmpty (Block c tx (Sealed c s))
    -> IO ()
switchToFork h@Handle{..} depth newChain =
    Sql.withTransaction hConn $ do
        rollbackBlocks h depth
        traverse_ (storeBlock' h) (toOldestFirst newChain)

lookupBlockTimestamp
    :: ToField (Hash c)
    => Connection
    -> BlockHash c
    -> IO (Maybe Timestamp)
lookupBlockTimestamp conn hsh =
    map fromOnly . listToMaybe <$>
        Sql.query conn [sql| SELECT timestamp FROM blocks WHERE hash = ? |] (Only hsh)

-- | Store a block in the block store.
--
-- Must be called from within a transaction. Must include a valid parent reference.
storeBlock'
    :: ( StorableTx c tx
       , ToField (Sealed c s)
       , Eq (Hash c)
       )
    => Handle c tx s
    -> Block c tx (Sealed c s)
    -> IO ()
storeBlock' Handle{..} blk = do
    parentTimestamp <- lookupBlockTimestamp hConn (blockPrevHash bh)

    -- Since we rely on the timestamp for block ordering, make sure this
    -- invariant is checked. Note that parent references are checked by the
    -- database.
    for_ parentTimestamp $ \t ->
        unless (blockTimestamp bh > t) $
            panic $ "Oscoin.Storage.Block.SQLite: "
                 <> "attempt to store block with invalid timestamp"

    -- Nb. To relate transactions with blocks, we store an extra block hash field
    -- for each row in the transactions table.
    Sql.execute hConn
        [sql| INSERT INTO blocks  (hash, timestamp, parenthash, datahash, statehash, difficulty, seal)
              VALUES              (?, ?, ?, ?, ?, ?, ?) |] row

    storeTxs hConn (blockHash blk) (toList $ blockData blk)

    -- Cache the block
    BlockCache.consBlock hBlockCache blk

  where
    row = ( blockHash blk
          , blockTimestamp bh
          , blockParent
          , blockDataHash bh
          , blockStateHash bh
          , blockTargetDifficulty bh
          , blockSeal bh
          )

    bh = blockHeader blk

    blockParent =
        if isGenesisBlock blk
           then Nothing
           else Just $ blockPrevHash bh


getBlocks
    :: ( Serialise s
       , StorableTx c tx
       , FromField (Hash c)
       , FromField (Sealed c s)
       )
    => Handle c tx s
    -> Depth
    -> IO (NewestFirst [] (Block c tx (Sealed c s)))
getBlocks Handle{..} (fromIntegral -> depth :: Int) =
    -- Hit the cache first
    BlockCache.cached hBlockCache depth (\backFillSize -> do
        rows :: [Only (BlockHash c) :. BlockHeader c (Sealed c s)] <- Sql.query hConn
            [sql|  SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                     FROM blocks
                 ORDER BY timestamp DESC
                    LIMIT ? |] (Only (fromIntegral backFillSize :: Integer))

        Chrono.NewestFirst <$>
            for rows (\(Only h :. bh) -> mkBlock bh <$> getBlockTxs hConn h))

getTip
    :: ( Serialise s
       , StorableTx c tx
       , FromField (Hash c)
       , FromField (Sealed c s)
       )
    => Handle c tx s
    -> IO (Block c tx (Sealed c s))
getTip h =
    headDef (panic "No blocks in storage!") . toNewestFirst <$> getBlocks h 1
