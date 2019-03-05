{-# LANGUAGE QuasiQuotes #-}

-- | Disk-backed block storage using SQLite.
module Oscoin.Storage.Block.SQLite
    ( withBlockStore
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (TxLookup(..))
import           Oscoin.Crypto.Blockchain.Block
                 ( Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Score
                 , Sealed
                 , mkBlock
                 )
import qualified Oscoin.Crypto.Hash as Crypto

import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.SQLite.Internal

import           Database.SQLite.Simple (Only(..))
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
                  , ToField (Crypto.Hash c)
                  , FromField (Crypto.Hash c)
                  , Serialise s
                  , Serialise (Crypto.Hash c)
                  , Ord s
                  , Ord (BlockHash c)
                  , ToRow tx
                  , FromRow tx
                  , Crypto.HasHashing c
                  )
               => String
               -- ^ The path where the DB will live on disk
               -> Block c tx (Sealed c s)
               -- ^ The genesis block (used to initialise the store)
               -> (Block c tx (Sealed c s) -> Score)
               -- ^ A block scoring function
               -> (Abstract.BlockStore c tx s IO -> IO b)
               -- ^ Action which uses the 'BlockStore'.
               -> IO b
withBlockStore path genesisBlock score action =
    let newBlockStore internalHandle =
            ( Abstract.BlockStore {
                  Abstract.scoreBlock      = hScoreFn internalHandle
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
    in bracket (newBlockStore <$> (initialize genesisBlock =<< open path score))
               (close . snd)
               (action . fst)


lookupBlock
    :: forall c tx s.
       ( Serialise s
       , Serialise (Crypto.Hash c)
       , ToField (Crypto.Hash c)
       , FromField (Sealed c s)
       , FromField (Crypto.Hash c)
       , FromRow tx
       , Crypto.HasHashing c
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
lookupTx
    :: ( Crypto.HasHashing c
       , FromRow tx
       , ToField (Crypto.Hashed c tx)
       )
    => Handle c tx s
    -> Crypto.Hashed c tx
    -> IO (Maybe (TxLookup c tx))
lookupTx Handle{hConn} h = do
    res <- Sql.query hConn
        [sql| SELECT message, author, chainid, nonce, context
                FROM transactions
               WHERE hash = ? |] (Only h)
    pure $ case listToMaybe res of
        Nothing -> Nothing
        Just tx -> Just $ TxLookup tx Crypto.zeroHash 0
