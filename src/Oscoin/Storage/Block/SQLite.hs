{-# LANGUAGE QuasiQuotes #-}

-- | Disk-backed block storage using SQLite.
module Oscoin.Storage.Block.SQLite
    ( withBlockStore
    , StorableTx
    , IsTxRow(..)
    , TxRow(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (TxLookup(..))
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHash, BlockHeader(..), Sealed, mkBlock)
import qualified Oscoin.Crypto.Hash as Crypto

import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.SQLite.Internal

import           Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.QQ
import           Database.SQLite.Simple.ToField (ToField)

import           Codec.Serialise (Serialise)

-- | Bracket-style initialisation of the SQLite BlockStore
-- FIXME(adn): This internally calls 'initialize', which is obviously not
-- ideal when we have state on disk and we are re-opening a database rather
-- than starting from scratch. In practice, /for now/, we should be OK as
-- 'initialize' should be idempotent, but nevertheless we need a better
-- migration strategy/initialisation here.
withBlockStore
    :: ( ToField s
       , FromField s
       , FromField (Crypto.Hash c)
       , Serialise s
       , StorableTx c tx
       )
    => String
    -- ^ The path where the DB will live on disk
    -> Block c tx (Sealed c s)
    -- ^ The genesis block (used to initialise the store)
    -> (Abstract.BlockStore c tx s IO -> IO b)
    -- ^ Action which uses the 'BlockStore'.
    -> IO b
withBlockStore path genesisBlock action =
    let newBlockStore internalHandle =
            ( (Abstract.BlockStoreReader {
                  Abstract.getGenesisBlock       = getGenesisBlock (hConn internalHandle)
                , Abstract.lookupBlock           = lookupBlock internalHandle
                , Abstract.lookupTx              = lookupTx internalHandle
                , Abstract.getBlocksByDepth      = getBlocks internalHandle
                , Abstract.getBlocksByParentHash = getChainSuffix (hConn internalHandle)
                , Abstract.getTip                = getTip internalHandle
                }
            , Abstract.BlockStoreWriter {
                  Abstract.insertBlock     = storeBlock internalHandle
                , Abstract.switchToFork    = switchToFork internalHandle
                })
            , internalHandle
            )
    in bracket (newBlockStore <$> (initialize genesisBlock =<< open path))
               (close . snd)
               (action . fst)


lookupBlock
    :: forall c tx s.
       ( Serialise s
       , FromField (Sealed c s)
       , FromField (Crypto.Hash c)
       , StorableTx c tx
       )
    => Handle c tx s
    -> BlockHash c
    -> IO (Maybe (Block c tx (Sealed c s)))
lookupBlock Handle{hConn} h = Sql.withTransaction hConn $ do
    result :: Maybe (BlockHeader c (Sealed c s)) <- listToMaybe <$> Sql.query hConn
        [sql| SELECT parenthash, datahash, statehash, timestamp, difficulty, seal
                FROM blocks
               WHERE hash = ? |] (Only h)

    txs :: [tx] <- getBlockTxs hConn h

    for result $ \bh ->
        pure $ mkBlock bh txs

-- FIXME(adn): At the moment there is no way to construct a proper 'TxLookup'
-- type out of the database.
lookupTx :: (StorableTx c tx) => Handle c tx s -> Crypto.Hashed c tx -> IO (Maybe (TxLookup c tx))
lookupTx Handle{hConn} hash = do
    maybeTx <- getTx hConn (Crypto.fromHashed hash)
    pure $ case maybeTx of
        Nothing -> Nothing
        Just tx -> Just $ TxLookup tx Crypto.zeroHash 0
