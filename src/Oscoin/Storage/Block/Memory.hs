{-# LANGUAGE OverloadedStrings #-}
-- | This module provides a in-memory implementation of
-- 'Abstract.BlockStore'.
--
-- The implementation is inefficient and only intended for testing.
module Oscoin.Storage.Block.Memory
    ( newBlockStoreIO
    , newBlockStoreFromGenesisIO
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain hiding (lookupTx)
import           Oscoin.Crypto.Blockchain.Block (Block)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Time.Chrono as Chrono

import           Data.IORef
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq

-- | Create an abstract blockstore backed by @'IORef' 'Seq'@.
newBlockStoreIO
    :: (MonadIO m, Crypto.Hashable c tx)
    => Chrono.NewestFirst NonEmpty (Block c tx (Sealed c s))
    -> IO (Abstract.BlockStore c tx s m)
newBlockStoreIO blks = do
    blocksRef <- newIORef $ Seq.fromList $ toList $ Chrono.toNewestFirst blks
    let getBlocks = liftIO $ readIORef blocksRef
    let modifyBlocks f = liftIO $ atomicModifyIORef' blocksRef $ \x -> (f x, ())
    pure $ mkBlockStore getBlocks modifyBlocks

newBlockStoreFromGenesisIO
    :: (MonadIO m, Crypto.Hashable c tx)
    => Block c tx (Sealed c s)
    -> IO (Abstract.BlockStore c tx s m)
newBlockStoreFromGenesisIO genesisBlk =
    newBlockStoreIO $ Chrono.NewestFirst (genesisBlk NonEmpty.:| [])

-- | Given two functions that are able to get and modify the state of
-- the underlying sequence of blocks we return a block store.
--
-- The underlying sequence has the newest block first.
mkBlockStore
    :: (Monad m, Crypto.Hashable c tx)
    => m (Seq (Block c tx (Sealed c s)))
    -> ((Seq (Block c tx (Sealed c s)) -> Seq (Block c tx (Sealed c s))) -> m ())
    -> Abstract.BlockStore c tx s m
mkBlockStore getBlocks modifyBlocks = (blockStoreReader, blockStoreWriter)
  where
    blockStoreReader = Abstract.BlockStoreReader
        { getGenesisBlock = do
            blks <- getBlocks
            case blks of
                Seq.Empty -> panic "Oscoin.Storage.Block.Memory: block store empty"
                _ Seq.:|> gen  -> pure gen
        , lookupBlock = \h ->
            find (\b -> blockHash b == h) <$> getBlocks
        , lookupBlockByHeight = \h ->
            find (\b -> (blockHeight . blockHeader $ b) == h) <$> getBlocks
        , lookupBlocksByHeight = \(start,end) ->
            -- As 'getBlocks' returns a 'Seq' and it's not guaranteed such
            -- sequence is non-empty and ordered by newest-blocks-first, we
            -- opt for a simple filter over the block height.
            Chrono.OldestFirst .  reverse .
            filter (\b -> (blockHeight . blockHeader $ b) >= start
                       && (blockHeight . blockHeader $ b) <= end
                   ) . toList <$> getBlocks
        , lookupTx = \txHash -> lookupTx txHash <$> getBlocks
        , getBlocksByDepth = \d ->
              Chrono.NewestFirst . take (fromIntegral d) . toList <$> getBlocks
        , getBlocksByParentHash = \h ->
              Chrono.NewestFirst . takeWhile (\b -> blockHash b /= h) . toList <$> getBlocks
        , getTip = do
            blks <- getBlocks
            case blks of
                Seq.Empty -> panic "Oscoin.Storage.Block.Memory: block store empty"
                tip' Seq.:<| _  -> pure tip'
        }
    blockStoreWriter = Abstract.BlockStoreWriter
        { insertBlock = \b -> modifyBlocks $ \bs -> b Seq.<| bs
        , switchToFork = \depth newSuffix -> modifyBlocks $ \blks ->
            Seq.fromList (toList $ Chrono.toNewestFirst $ Chrono.reverse newSuffix) <> Seq.drop (fromIntegral depth) blks
        }

lookupTx :: (Crypto.Hashable crypto tx) => Crypto.Hashed crypto tx -> Seq (Block c tx s) -> Maybe (TxLookup c tx)
lookupTx txHash blks =
    getFirst $ foldMap (First . matchTxLookup) blocksWithDepth
  where
    blocksWithDepth = zip [1..] $ toList blks
    matchTxLookup (depth, blk) = do
        tx <- findTxInBlock blk
        Just $ TxLookup tx (blockHash blk) depth
    findTxInBlock blk = find (\tx -> Crypto.hash tx == txHash) (blockTxs blk)
