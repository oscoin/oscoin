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
    , runTransaction
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
    , lookupBlock
    , lookupTx
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Depth, TxLookup(..))
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
import           Oscoin.Crypto.Hash (Hash, Hashed, fromHashed, zeroHash)
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

-- | Simple, composable transaction mechanism to avoid shooting ourselves in the
-- foot when working with MVars. It ensures that 'withMVar' is called in a
-- single place ('runTransaction') rather than being scattered in multiple places.
-- It also allows transactions to be composed together.
newtype Transaction m a = TransactionT (ReaderT Connection m a)
    deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO)

-- | A handle to an on-disk block store.
data Handle c tx s = Handle
    { hConn       :: MVar Connection               -- ^ Connection to on-disk storage for non-orphan blocks.
    , hBlockCache :: BlockCache c tx (Sealed c s)  -- ^ In-memory cache for most recent blocks.
    }

-- | Runs a 'Transaction' by acquiring an exclusive lock on the underlying
-- 'Connection' and running the whole action in a single SQLite transaction.
runTransaction :: MVar Connection -> Transaction IO a -> IO a
runTransaction connVar = runTransactionWith Sql.withTransaction connVar

runTransactionWith
    :: (Connection -> IO a -> IO a)
    -> MVar Connection
    -> Transaction IO a
    -> IO a
runTransactionWith finaliser connVar (TransactionT (ReaderT m)) =
    withMVar connVar $ \conn ->
        flip runReaderT conn $
           liftIO (finaliser conn (m conn))

-- | Opens a connection to the SQLite DB.
open :: String -> IO (Handle c tx s)
open path =
    Handle <$> (Sql.open path >>= newMVar >>= setupConnection)
           <*> BlockCache.newBlockCache 2016 -- TODO(adn) pass this parameter externally
  where
    setupConnection connVar = do
        runTransaction connVar enableForeignKeys
        pure connVar
    enableForeignKeys = do
        conn <- ask
        liftIO $ Sql3.exec (Sql.connectionHandle conn) "PRAGMA foreign_keys = ON;"

-- | Close a connection to the block store.
-- NOTE(adn) You shouldn't call 'runTransaction' here, as that would trigger
-- a SQLite 'ErrorMisure' error.
close :: Handle c tx s -> IO ()
close Handle{..} = runTransactionWith (const identity) hConn $ do
    conn <- ask
    liftIO $ Sql.close conn

-- | Initialize the block store with a genesis block. This can safely be run
-- multiple times.
initialize
    :: ( StorableTx c tx
       , ToField (Sealed c s)
       )
    => Block c tx (Sealed c s)
    -> Handle c tx s
    -> IO (Handle c tx s)
initialize gen h@Handle{hConn} = runTransaction hConn $ do
    conn   <- ask
    liftIO $
        getDataFileName "data/blockstore.sql" >>= readFile >>= Sql3.exec (Sql.connectionHandle conn)
    unlessM (isStored (blockHash gen)) $
        storeBlock' (hBlockCache h) gen
    pure h

-- | Check whether a given block hash exists.
isStored
    :: ToField (Hash c)
    => BlockHash c
    -> Transaction IO Bool
isStored bh = do
    conn <- ask
    headDef False . map fromOnly <$> liftIO (Sql.query conn
        [sql| SELECT EXISTS (SELECT 1 FROM blocks WHERE hash = ?) |] (Only bh))

-- | Check if a given block is conflicting with the main chain. This means a
-- block already exists with the same parent.
isConflicting
    :: ToField (Hash c)
    => Block c tx s
    -> Transaction IO Bool
isConflicting block = do
    conn <- ask
    headDef False . map fromOnly <$> liftIO (Sql.query conn
        [sql| SELECT EXISTS (SELECT 1 FROM blocks WHERE parenthash = ?) |]
        (Only $ blockPrevHash $ blockHeader block))

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
        ) => Handle c tx s
          -> Block c tx (Sealed c s)
          -> IO ()
storeBlock Handle{..} blk = runTransaction hConn $ do
    blockExists        <- isStored $ blockHash blk
    blockConflicts     <- isConflicting blk
    currentTip         <- getTipInternal hBlockCache

    if not blockExists && extendsTip currentTip && not blockConflicts
        then storeBlock' hBlockCache blk
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
    => Handle c tx s
    -> IO (Block c tx (Sealed c s))
getGenesisBlock Handle{hConn} = runTransaction hConn $ getGenesisBlockInternal

getGenesisBlockInternal
    :: forall c s tx.
       ( Serialise s
       , FromField s
       , StorableTx c tx
       , FromField (Hash c)
       )
    => Transaction IO (Block c tx s)
getGenesisBlockInternal = do
    conn <- ask
    Only bHash :. bHeader <-
        headDef (panic "No genesis block!") <$> liftIO (Sql.query_ conn
            [sql|   SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                      FROM blocks
                     WHERE parenthash IS NULL |])
    bTxs <- liftIO (getBlockTxs @c conn bHash)
    pure $ mkBlock bHeader bTxs

-- | Get the chain starting from a given hash up to the tip
getChainSuffix
    :: ( StorableTx c tx
       , Serialise s
       , FromField s
       , FromField (Hash c)
       )
    => Handle c tx s
    -> BlockHash c
    -> IO (NewestFirst [] (Block c tx (Sealed c s)))
getChainSuffix Handle{hConn} hsh =
    runTransaction hConn $ getChainSuffixInternal hsh

getChainSuffixInternal
    :: ( StorableTx c tx
       , Serialise s
       , FromField s
       , FromField (Hash c)
       )
    => BlockHash c
    -> Transaction IO (NewestFirst [] (Block c tx s))
getChainSuffixInternal hsh = do
    result <- lookupBlockTimestamp hsh
    case result of
        Nothing -> pure mempty
        Just t -> do
            conn <- ask
            liftIO $ do
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
rollbackBlocks
    :: BlockCache c tx (Sealed c s)
    -> Depth
    -> Transaction IO ()
rollbackBlocks blockCache (fromIntegral -> depth :: Integer) = do
    conn <- ask
    liftIO $ do
        Sql.execute conn
            [sql| DELETE FROM blocks WHERE hash IN
                  (SELECT hash from blocks ORDER BY timestamp DESC LIMIT ?)
            |] (Only depth)
        BlockCache.invalidate blockCache

switchToFork
    :: ( StorableTx c tx
       , ToField s
       )
    => Handle c tx s
    -> Depth
    -> OldestFirst NonEmpty (Block c tx (Sealed c s))
    -> IO ()
switchToFork Handle{..} depth newChain = runTransaction hConn $ do
    rollbackBlocks hBlockCache depth
    traverse_ (storeBlock' hBlockCache) (toOldestFirst newChain)

lookupBlockTimestamp
    :: ToField (Hash c)
    => BlockHash c
    -> Transaction IO (Maybe Timestamp)
lookupBlockTimestamp hsh = do
    conn <- ask
    liftIO $
        map fromOnly . listToMaybe <$>
            Sql.query conn [sql| SELECT timestamp FROM blocks WHERE hash = ? |] (Only hsh)

-- | Store a block in the block store.
--
-- Must be called from within a transaction. Must include a valid parent reference.
storeBlock'
    :: ( StorableTx c tx
       , ToField (Sealed c s)
       )
    => BlockCache c tx (Sealed c s)
    -> Block c tx (Sealed c s)
    -> Transaction IO ()
storeBlock' hBlockCache blk = do
    hConn <- ask
    parentTimestamp <- lookupBlockTimestamp (blockPrevHash bh)

    -- Since we rely on the timestamp for block ordering, make sure this
    -- invariant is checked. Note that parent references are checked by the
    -- database.
    for_ parentTimestamp $ \t ->
        unless (blockTimestamp bh > t) $
            panic $ "Oscoin.Storage.Block.SQLite: "
                 <> "attempt to store block with invalid timestamp"

    -- Nb. To relate transactions with blocks, we store an extra block hash field
    -- for each row in the transactions table.
    liftIO $ do
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
getBlocks Handle{..} depth = runTransaction hConn $
    getBlocksInternal hBlockCache depth

getBlocksInternal
    :: ( Serialise s
       , StorableTx c tx
       , FromField (Hash c)
       , FromField (Sealed c s)
       )
    => BlockCache c tx (Sealed c s)
    -> Depth
    -> Transaction IO (NewestFirst [] (Block c tx (Sealed c s)))
getBlocksInternal hBlockCache (fromIntegral -> depth :: Int) = do
    conn <- ask
    -- Hit the cache first
    liftIO $ BlockCache.cached hBlockCache depth (\backFillSize -> do
        rows :: [Only (BlockHash c) :. BlockHeader c (Sealed c s)] <- Sql.query conn
            [sql|  SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                     FROM blocks
                 ORDER BY timestamp DESC
                    LIMIT ? |] (Only (fromIntegral backFillSize :: Integer))

        Chrono.NewestFirst <$>
            for rows (\(Only h :. bh) -> mkBlock bh <$> getBlockTxs conn h))

getTip
    :: ( Serialise s
       , StorableTx c tx
       , FromField (Hash c)
       , FromField (Sealed c s)
       )
    => Handle c tx s
    -> IO (Block c tx (Sealed c s))
getTip h = runTransaction (hConn h) $ getTipInternal (hBlockCache h)

getTipInternal
    :: ( Serialise s
       , StorableTx c tx
       , FromField (Hash c)
       , FromField (Sealed c s)
       )
    => BlockCache c tx (Sealed c s)
    -> Transaction IO (Block c tx (Sealed c s))
getTipInternal cache =
    headDef (panic "No blocks in storage!") . toNewestFirst <$> getBlocksInternal cache 1

lookupBlock
    :: forall c tx s.
       ( Serialise s
       , FromField (Sealed c s)
       , FromField (Hash c)
       , StorableTx c tx
       )
    => Handle c tx s
    -> BlockHash c
    -> IO (Maybe (Block c tx (Sealed c s)))
lookupBlock Handle{hConn} h = runTransaction hConn $ do
    conn <- ask
    result :: Maybe (BlockHeader c (Sealed c s)) <- listToMaybe <$> liftIO (Sql.query conn
        [sql| SELECT parenthash, datahash, statehash, timestamp, difficulty, seal
                FROM blocks
               WHERE hash = ? |] (Only h))

    txs :: [tx] <- liftIO $ getBlockTxs conn h

    for result $ \bh ->
        pure $ mkBlock bh txs

-- FIXME(adn): At the moment there is no way to construct a proper 'TxLookup'
-- type out of the database.
lookupTx
    :: StorableTx c tx
    => Handle c tx s
    -> Hashed c tx
    -> IO (Maybe (TxLookup c tx))
lookupTx Handle{hConn} hash = runTransaction hConn $ do
    conn <- ask
    maybeTx <- liftIO $ getTx conn (fromHashed hash)
    pure $ case maybeTx of
        Nothing -> Nothing
        Just tx -> Just $ TxLookup tx zeroHash 0
