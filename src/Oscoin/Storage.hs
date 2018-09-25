module Oscoin.Storage
    ( ApplyResult(..)

    , applyBlock
    , applyTx
    , lookupBlock
    , lookupTx

    , isNovelTx
    , isNovelBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.Consensus.Evaluator (Evaluator)

import           Oscoin.Crypto.Blockchain hiding (lookupTx)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool

import           Control.Monad.Trans.Maybe (MaybeT(..))

data ApplyResult =
      Applied
    | Stale
    | Error -- TODO(kim): figure out informative error types
    deriving Eq

applyBlock
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       )
    => Evaluator s tx ()
    -> Block       tx ()
    -> m ApplyResult
applyBlock eval blk = do
    novel <- isNovelBlock (blockHash blk)
    if | novel ->
        case validateBlock blk of
            Left  _    -> pure Error
            Right blk' -> do
                BlockStore.storeBlock $ toOrphan eval blk'
                Mempool.delTxs (blockData blk')
                pure Applied

       | otherwise -> pure Stale

applyTx
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       , Crypto.Hashable tx
       )
    => tx
    -> m ApplyResult
applyTx tx = do
    novel <- isNovelTx (Crypto.hash tx)
    if | novel     -> Mempool.addTxs [tx] $> Applied
       | otherwise -> pure Stale

lookupBlock :: MonadBlockStore tx s m => BlockHash -> m (Maybe (Block tx ()))
lookupBlock hsh = (map . map) void $ BlockStore.lookupBlock hsh

lookupTx
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       )
    => Hashed tx
    -> m (Maybe tx)
lookupTx hsh =
    runMaybeT $
            MaybeT (BlockStore.lookupTx hsh)
        <|> MaybeT (Mempool.lookupTx hsh)

--------------------------------------------------------------------------------

isNovelBlock :: (MonadBlockStore tx s m) => BlockHash -> m Bool
isNovelBlock h =
    isNothing <$> BlockStore.lookupBlock h

isNovelTx :: (MonadBlockStore tx s m, MonadMempool tx m) => Hashed tx -> m Bool
isNovelTx h = do
    inBlockStore <- BlockStore.lookupTx h
    inMempool    <- Mempool.lookupTx h
    pure . isNothing $ inBlockStore <|> inMempool
