{-# LANGUAGE QuasiQuotes #-}

-- | Disk-backed block storage using SQLite.
module Oscoin.Storage.Block.SQLite
    ( withBlockStore
    , StorableTx
    , SQLite.TxRow(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (TxLookup(..))
import           Oscoin.Crypto.Blockchain.Block
                 ( Beneficiary
                 , Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Sealed
                 , mkBlock
                 )
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.OscoinTx (Tx)

import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.SQLite.Internal (Handle(..))
import qualified Oscoin.Storage.Block.SQLite.Internal as SQLite
import           Oscoin.Storage.Block.SQLite.Internal.Tx (StorableTx)

import           Database.SQLite.Simple ((:.)(..), Only(..))
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
       , Typeable c
       , Serialise s
       , StorableTx c
       )
    => String
    -- ^ The path where the DB will live on disk
    -> Block c (Tx c) (Sealed c s)
    -- ^ The genesis block (used to initialise the store)
    -> (Abstract.BlockStore c (Tx c) s IO -> IO b)
    -- ^ Action which uses the 'BlockStore'.
    -> IO b
withBlockStore path genesisBlock action =
    let newBlockStore internalHandle =
            ( (Abstract.BlockStoreReader {
                  Abstract.getGenesisBlock       = SQLite.getGenesisBlock internalHandle
                , Abstract.lookupBlock           = lookupBlock internalHandle
                , Abstract.lookupBlockByHeight   = SQLite.lookupBlockByHeight internalHandle
                , Abstract.lookupBlocksByHeight  = SQLite.lookupBlocksByHeight internalHandle
                , Abstract.lookupTx              = lookupTx internalHandle
                , Abstract.getBlocksByDepth      = SQLite.getBlocks internalHandle
                , Abstract.getBlocksByParentHash = SQLite.getChainSuffix internalHandle
                , Abstract.getTip                = SQLite.getTip internalHandle
                }
            , Abstract.BlockStoreWriter {
                  Abstract.insertBlock     = SQLite.storeBlock internalHandle
                , Abstract.switchToFork    = SQLite.switchToFork internalHandle
                })
            , internalHandle
            )
    in bracket (newBlockStore <$> (SQLite.initialize genesisBlock =<< SQLite.open path))
               (SQLite.close . snd)
               (action . fst)


lookupBlock
    :: forall c s.
       ( Serialise s
       , Typeable c
       , FromField (Sealed c s)
       , StorableTx c
       )
    => Handle c (Tx c) s
    -> BlockHash c
    -> IO (Maybe (Block c (Tx c) (Sealed c s)))
lookupBlock Handle{hConn} h = SQLite.runTransaction hConn $ do
    conn <- ask
    result :: Maybe (BlockHeader c (Sealed c s) :. Only (Beneficiary c)) <- listToMaybe <$> liftIO (Sql.query conn
        [sql| SELECT height, parenthash, datahash, statehash, timestamp, difficulty, seal, beneficiary
                FROM blocks
               WHERE hash = ? |] (Only h))

    txs :: [Tx c] <- liftIO $ SQLite.getBlockTxs conn h

    for result $ \(bh :. Only be) ->
        pure $ mkBlock bh be txs

-- FIXME(adn): At the moment there is no way to construct a proper 'TxLookup'
-- type out of the database.
lookupTx
    :: (StorableTx c, Typeable c)
    => Handle c (Tx c) s -> Crypto.Hashed c (Tx c) -> IO (Maybe (TxLookup c (Tx c)))
lookupTx Handle{hConn} hash = SQLite.runTransaction hConn $ do
    conn <- ask
    maybeTx <- liftIO $ SQLite.lookupTx conn (Crypto.fromHashed hash)
    pure $ case maybeTx of
        Nothing -> Nothing
        Just tx -> Just $ TxLookup tx Crypto.zeroHash 0
