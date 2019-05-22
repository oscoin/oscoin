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
    , lookupBlockByHeight
    , lookupBlocksByHeight
    , switchToFork
    , storeBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Depth)
import           Oscoin.Crypto.Blockchain.Block
                 ( Beneficiary
                 , Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Height
                 , Sealed
                 , blockBeneficiary
                 , blockHash
                 , blockTxs
                 , isGenesisBlock
                 , mkBlock
                 , parentHash
                 )
import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Storage.Block.SQLite.Transaction
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
       , Serialise (Beneficiary c)
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
-- block already exists with the same height.
isConflicting
    :: Block c tx s
    -> Transaction IO Bool
isConflicting block = do
    conn <- ask
    headDef False . map fromOnly <$> liftIO (Sql.query conn
        [sql| SELECT EXISTS (SELECT 1 FROM blocks WHERE height = ?) |]
        (Only $ blockHeight $ blockHeader block))

-- | Store a block, along with its transactions in the block store.
-- Invariant: the input block must have passed \"basic\" validation.
-- (cfr. 'applyBlock').
storeBlock
    :: forall c tx s.
        ( ToField (Sealed c s)
        , StorableTx c tx
        , Serialise (Beneficiary c)
        , Serialise s
        , Typeable c
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
    extendsTip currentTip =
        (blockHash currentTip              == parentHash blk) &&
        ((blockHeight . blockHeader $ blk) == (succ . blockHeight . blockHeader $ currentTip))


getGenesisBlock
    :: forall c s tx.
       ( Serialise s
       , Serialise (Beneficiary c)
       , Typeable c
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
       , Serialise (Beneficiary c)
       , Typeable c
       , FromField s
       , StorableTx c tx
       , FromField (Hash c)
       )
    => Transaction IO (Block c tx s)
getGenesisBlockInternal = do
    conn <- ask
    Only bHash :. bHeader :. Only bBeneficiary <-
        headDef (panic "No genesis block!") <$> liftIO (Sql.query_ conn
            [sql|   SELECT hash, height, parenthash, datahash, statehash, timestamp, difficulty, seal, beneficiary
                      FROM blocks
                     WHERE parenthash IS NULL |])
    bTxs <- liftIO (getBlockTxs @c conn bHash)
    pure $ mkBlock bHeader bBeneficiary bTxs

-- | Get the chain starting from a given hash up to the tip
getChainSuffix
    :: ( StorableTx c tx
       , Serialise s
       , Serialise (Beneficiary c)
       , Typeable c
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
       , Serialise (Beneficiary c)
       , Typeable c
       , FromField s
       , FromField (Hash c)
       )
    => BlockHash c
    -> Transaction IO (NewestFirst [] (Block c tx s))
getChainSuffixInternal hsh = do
    result <- lookupBlockHeight hsh
    case result of
        Nothing -> pure mempty
        Just height -> do
            conn <- ask
            liftIO $ do
                rows :: [Only (BlockHash c) :. BlockHeader c s :. Only (Beneficiary c)] <- Sql.query conn
                    [sql|  SELECT hash, height, parenthash, datahash, statehash, timestamp, difficulty, seal, beneficiary
                             FROM blocks WHERE height > ?
                         ORDER BY height DESC
                            |] (Only height)
                map NewestFirst <$> for rows $ \(Only h :. bh :. Only be) ->
                    mkBlock bh be <$> getBlockTxs conn h

getChainHashes
    :: FromField (Hash c)
    => Connection
    -> Depth
    -> IO [BlockHash c]
getChainHashes conn (fromIntegral -> depth :: Integer) =
    map fromOnly <$>
      Sql.query conn [sql| SELECT hash FROM blocks ORDER BY height DESC LIMIT ? |] (Only depth)

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
                  (SELECT hash from blocks ORDER BY height DESC LIMIT ?)
            |] (Only depth)
        BlockCache.invalidate blockCache

switchToFork
    :: ( StorableTx c tx
       , ToField s
       , Serialise (Beneficiary c)
       )
    => Handle c tx s
    -> Depth
    -> OldestFirst NonEmpty (Block c tx (Sealed c s))
    -> IO ()
switchToFork Handle{..} depth newChain = runTransaction hConn $ do
    rollbackBlocks hBlockCache depth
    traverse_ (storeBlock' hBlockCache) (toOldestFirst newChain)

lookupBlockHeight
    :: ToField (Hash c)
    => BlockHash c
    -> Transaction IO (Maybe Height)
lookupBlockHeight hsh = do
    conn <- ask
    liftIO $
        map fromOnly . listToMaybe <$>
            Sql.query conn [sql| SELECT height FROM blocks WHERE hash = ? |] (Only hsh)

-- | Store a block in the block store.
--
-- Must be called from within a transaction. Must include a valid parent reference.
-- /precondition:/ This block must extend the tip.
storeBlock'
    :: ( StorableTx c tx
       , ToField (Sealed c s)
       , Serialise (Beneficiary c)
       )
    => BlockCache c tx (Sealed c s)
    -> Block c tx (Sealed c s)
    -> Transaction IO ()
storeBlock' hBlockCache blk = do
    hConn <- ask
    parentHeight <- lookupBlockHeight (blockPrevHash bh)

    -- Enforce the precondition by checking the parent height.
    for_ parentHeight $ \prevHeight ->
        unless (blockHeight bh > prevHeight) $
            panic $ "Oscoin.Storage.Block.SQLite: "
                 <> "attempt to store block with invalid height (<= of tip)."

    -- Nb. To relate transactions with blocks, we store an extra block hash field
    -- for each row in the transactions table.
    liftIO $ do
        Sql.execute hConn
            [sql| INSERT INTO blocks  (hash, timestamp, height, parenthash, datahash, statehash, difficulty, seal, beneficiary)
                  VALUES              (?, ?, ?, ?, ?, ?, ?, ?, ?) |] row

        storeTxs hConn (blockHash blk) (toList $ blockTxs blk)

        -- Cache the block
        BlockCache.consBlock hBlockCache blk

  where
    row = ( blockHash blk
          , blockTimestamp bh
          , blockHeight bh
          , blockParent
          , blockDataHash bh
          , blockStateHash bh
          , blockTargetDifficulty bh
          , blockSeal bh
          , blockBeneficiary blk
          )

    bh = blockHeader blk

    blockParent =
        if isGenesisBlock blk
           then Nothing
           else Just $ blockPrevHash bh


getBlocks
    :: ( Serialise s
       , StorableTx c tx
       , Serialise (Beneficiary c)
       , Typeable c
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
       , Serialise (Beneficiary c)
       , StorableTx c tx
       , Typeable c
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
        rows :: [Only (BlockHash c) :. BlockHeader c (Sealed c s) :. Only (Beneficiary c)] <- Sql.query conn
            [sql|  SELECT hash, height, parenthash, datahash, statehash, timestamp, difficulty, seal, beneficiary
                     FROM blocks
                 ORDER BY height DESC
                    LIMIT ? |] (Only (fromIntegral backFillSize :: Integer))

        Chrono.NewestFirst <$>
            for rows (\(Only h :. bh :. Only be) -> mkBlock bh be <$> getBlockTxs conn h))

getTip
    :: ( Serialise s
       , Serialise (Beneficiary c)
       , StorableTx c tx
       , Typeable c
       , FromField (Hash c)
       , FromField (Sealed c s)
       )
    => Handle c tx s
    -> IO (Block c tx (Sealed c s))
getTip h = runTransaction (hConn h) $ getTipInternal (hBlockCache h)

getTipInternal
    :: ( Serialise s
       , Serialise (Beneficiary c)
       , StorableTx c tx
       , Typeable c
       , FromField (Hash c)
       , FromField (Sealed c s)
       )
    => BlockCache c tx (Sealed c s)
    -> Transaction IO (Block c tx (Sealed c s))
getTipInternal cache =
    headDef (panic "No blocks in storage!") . toNewestFirst <$> getBlocksInternal cache 1

lookupBlockByHeight
    :: forall c tx s.
       ( Serialise s
       , Serialise (Beneficiary c)
       , Typeable c
       , FromField (Sealed c s)
       , FromField (Hash c)
       , StorableTx c tx
       )
    => Handle c tx s
    -> Height
    -> IO (Maybe (Block c tx (Sealed c s)))
lookupBlockByHeight Handle{hConn} h = runTransaction hConn $ do
    conn <- ask
    result :: Maybe (BlockHeader c (Sealed c s) :. Only (Beneficiary c) :. Only (BlockHash c)) <- listToMaybe <$> liftIO (Sql.query conn
        [sql| SELECT height, parenthash, datahash, statehash, timestamp, difficulty, seal, beneficiary
                FROM blocks
               WHERE hash = ? |] (Only h))

    for result $ \(bh :. Only be :. Only blockHash) -> do
        txs :: [tx] <- liftIO $ getBlockTxs conn blockHash
        pure $ mkBlock bh be txs

lookupBlocksByHeight
    :: forall c tx s.
       ( Serialise s
       , Serialise (Beneficiary c)
       , Typeable c
       , FromField (Sealed c s)
       , FromField (Hash c)
       , StorableTx c tx
       )
    => Handle c tx s
    -> (Height, Height)
    -- ^ The /start/ and the /end/ of the range (inclusive).
    -> IO (OldestFirst [] (Block c tx (Sealed c s)))
lookupBlocksByHeight Handle{hConn} (start,end) = runTransaction hConn $ do
    conn <- ask
    liftIO $ do
        rows :: [Only (BlockHash c) :. BlockHeader c (Sealed c s) :. Only (Beneficiary c)] <- Sql.query conn
            [sql|  SELECT hash, height, parenthash, datahash, statehash, timestamp, difficulty, seal, beneficiary
                     FROM blocks
                     WHERE height >= ? AND height <= ?
                 ORDER BY height ASC
                   |] (Only start :. Only end)

        Chrono.OldestFirst <$>
            for rows (\(Only h :. bh :. Only be) -> mkBlock bh be <$> getBlockTxs conn h)
