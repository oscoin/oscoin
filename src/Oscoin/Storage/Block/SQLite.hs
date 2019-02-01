{-# LANGUAGE QuasiQuotes #-}

-- | Disk-backed block storage using SQLite.
module Oscoin.Storage.Block.SQLite
    ( withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Validate)
import           Oscoin.Crypto.Blockchain (TxLookup(..))
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHash, BlockHeader(..), Score, mkBlock)
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
-- FIXME(adn): This internally calls 'initialize', which is obviously not
-- ideal when we have state on disk and we are re-opening a database rather
-- than starting from scratch. In practice, /for now/, we should be OK as
-- 'initialize' should be idempotent, but nevertheless we need a better
-- migration strategy/initialisation here.
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
               -- ^ Action which uses the 'BlockStore'.
               -> IO b
withBlockStore path genesisBlock score validate action =
    let newBlockStore internalHandle =
            ( Abstract.BlockStore {
                  Abstract.scoreBlock      = hScoreFn internalHandle
                , Abstract.validateBlock   = hValidFn internalHandle
                , Abstract.insertBlock     = storeBlock internalHandle
                , Abstract.getGenesisBlock = getGenesisBlock (hConn internalHandle)
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

-- FIXME(adn): At the moment there is no way to construct a proper 'TxLookup'
-- type out of the database.
lookupTx :: FromRow tx => Handle tx s -> Crypto.Hashed tx -> IO (Maybe (TxLookup tx))
lookupTx Handle{hConn} h = do
    res <- Sql.query hConn
        [sql| SELECT message, author, chainid, nonce, context
                FROM transactions
               WHERE hash = ? |] (Only h)
    pure $ case listToMaybe res of
        Nothing -> Nothing
        Just tx -> Just $ TxLookup tx Crypto.zeroHash 0
