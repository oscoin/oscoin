{-# LANGUAGE QuasiQuotes #-}

-- | Disk-backed block storage using SQLite.
module Oscoin.Storage.Block.SQLite
    ( withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Validate)
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHash, BlockHeader(..), Depth, Score, mkBlock)
import qualified Oscoin.Crypto.Hash as Crypto

import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.SQLite.Internal

import           Database.SQLite.Simple ((:.)(..), Only(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow (FromRow)
import           Database.SQLite.Simple.QQ
import           Database.SQLite.Simple.ToField (ToField)
import           Database.SQLite.Simple.ToRow (ToRow)

import           Codec.Serialise (Serialise)

-- | Bracket-style initialisation of the SQLite BlockStore
withBlockStore :: ( ToField s
                  , FromField s
                  , Serialise s
                  , Ord s
                  , ToRow tx
                  , FromRow tx
                  , Ord tx)
               => String
               -- ^ The path where the DB will live on disk
               -> Block tx s
               -- ^ The genesis block (used to initialise the store)
               -> (Block tx s -> Score)
               -- ^ A block scoring function
               -> Validate tx s
               -- ^ A block validation function
               -> (Abstract.BlockStore tx s IO -> IO b)
               -- ^ Action to use the 'BlockStore'.
               -> IO b
withBlockStore path genesisBlock score validate action =
    let newBlockStore internalHandle =
            ( Abstract.BlockStore {
                  Abstract.scoreBlock      = hScoreFn internalHandle
                , Abstract.validateBlock   = hValidFn internalHandle
                , Abstract.insertBlock     = storeBlock internalHandle
                , Abstract.getGenesisBlock = getGenesisBlock (hConn internalHandle)
                , Abstract.member          = isStored (hConn internalHandle)
                , Abstract.lookupBlock     = lookupBlock internalHandle
                , Abstract.lookupTx        = lookupTx internalHandle
                , Abstract.getOrphans      = getOrphans internalHandle
                , Abstract.getBlocks       = getBlocks internalHandle
                , Abstract.getTip          = getTip internalHandle
                }
            , internalHandle
            )
    in bracket (newBlockStore <$> (initialize genesisBlock =<< open path score validate))
               (close . snd)
               (action . fst)


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
