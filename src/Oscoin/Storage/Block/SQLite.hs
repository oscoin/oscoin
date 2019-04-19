-- | Disk-backed block storage using SQLite.
module Oscoin.Storage.Block.SQLite
    ( withBlockStore
    , StorableTx
    , IsTxRow(..)
    , TxRow(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (Block(..), Sealed)
import qualified Oscoin.Crypto.Hash as Crypto

import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.SQLite.Internal as SQLite

import           Database.SQLite.Simple.FromField (FromField)
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
                  Abstract.getGenesisBlock       = getGenesisBlock internalHandle
                , Abstract.lookupBlock           = lookupBlock internalHandle
                , Abstract.lookupTx              = lookupTx internalHandle
                , Abstract.getBlocksByDepth      = getBlocks internalHandle
                , Abstract.getBlocksByParentHash = getChainSuffix internalHandle
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
