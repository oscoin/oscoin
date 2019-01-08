{-# LANGUAGE QuasiQuotes #-}

-- | Disk-backed block storage using SQLite.
module Oscoin.Storage.Block.SQLite
    ( Handle
    , withBlockStore
    , open
    , close
    , initialize
    , storeBlock
    , lookupBlock
    , lookupTx
    , getGenesisBlock
    , getBlocks
    , getTip
    , getOrphans
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Validate)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Depth
                 , Score
                 , blockHash
                 , mkBlock
                 )
import qualified Oscoin.Crypto.Hash as Crypto

import           Oscoin.Storage.Block.SQLite.Internal

import           Oscoin.ProtocolConfig (ProtocolConfig)

import           Paths_oscoin

import           Control.Concurrent.STM

import           Database.SQLite.Simple ((:.)(..), Only(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow (FromRow)
import           Database.SQLite.Simple.Orphans ()
import           Database.SQLite.Simple.QQ
import           Database.SQLite.Simple.ToField (ToField)
import           Database.SQLite.Simple.ToRow (ToRow)
import qualified Database.SQLite3 as Sql3

import           Codec.Serialise (Serialise)
import qualified Data.Set as Set

-- | Open a connection to the block store.
open :: (Ord s, Ord tx)
     => String
     -> (Block tx s -> Score)
     -> Validate tx s
     -> ProtocolConfig
     -> IO (Handle tx s)
open path score validate protocolConfig =
    Handle <$> (Sql.open path >>= setupConnection)
           <*> newTVarIO mempty
           <*> pure score
           <*> pure validate
           <*> pure protocolConfig
  where
    setupConnection conn = do
        enableForeignKeys conn
        pure conn
    enableForeignKeys conn =
        Sql3.exec (Sql.connectionHandle conn) "PRAGMA foreign_keys = ON;"

-- | Close a connection to the block store.
close :: Handle tx s -> IO ()
close Handle{..} = Sql.close hConn

withBlockStore :: (Ord s, Ord tx)
               => String
               -> (Block tx s -> Score)
               -> Validate tx s
               -> ProtocolConfig
               -> (Handle tx s -> IO b)
               -> IO b
withBlockStore path score validate protocolConfig =
    bracket (open path score validate protocolConfig) close

-- | Initialize the block store with a genesis block. This can safely be run
-- multiple times.
initialize :: (ToRow tx, ToField s) => Block tx s -> Handle tx s -> IO (Handle tx s)
initialize gen h@Handle{hConn} =
    getDataFileName "data/blockstore.sql" >>=
        readFile >>=
        Sql3.exec (Sql.connectionHandle hConn) >>
            storeBlock' h gen >>
                pure h

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

lookupBlock :: forall tx s. (Serialise s, FromField s, FromRow tx) => Handle tx s -> BlockHash -> IO (Maybe (Block tx s))
lookupBlock Handle{hConn} h = Sql.withTransaction hConn $ do
    result :: Maybe (BlockHeader s) <- listToMaybe <$> Sql.query hConn
        [sql| SELECT parenthash, datahash, statehash, timestamp, difficulty, seal
                FROM blocks
               WHERE hash = ? |] (Only h)

    txs :: [tx] <- getBlockTxs hConn h

    for result $ \bh ->
        pure $ mkBlock bh txs

lookupTx :: FromRow tx => Handle tx s -> Crypto.Hashed tx -> IO (Maybe tx)
lookupTx Handle{hConn} h =
    listToMaybe <$> Sql.query hConn
        [sql| SELECT message, author, chainid, nonce, context
                FROM transactions
               WHERE hash = ? |] (Only h)

getGenesisBlock :: (Serialise s, FromField s, FromRow tx) => Handle tx s -> IO (Block tx s)
getGenesisBlock Handle{hConn} = do
    Only bHash :. bHeader <-
        headDef (panic "No genesis block!") <$> Sql.query_ hConn
            [sql|   SELECT hash, parenthash, datahash, statehash, timestamp, difficulty, seal
                      FROM blocks
                     WHERE parenthash IS NULL |]
    bTxs <- getBlockTxs hConn bHash
    pure $ mkBlock bHeader bTxs

getOrphans :: Handle tx s -> IO (Set BlockHash)
getOrphans Handle{..} =
    Set.map blockHash <$> readTVarIO hOrphans

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
