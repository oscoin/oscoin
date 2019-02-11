module Oscoin.Consensus.Mining
    ( mineBlock
    , mineGenesis
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Block.Abstract (BlockStore)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.State.Class (MonadStateStore)
import qualified Oscoin.Storage.State.Class as StateStore

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval (Evaluator, buildBlock)
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Storage.Receipt.Class

import           Codec.Serialise (Serialise)

-- | Mine a block with the given 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadMempool      tx   m
       , MonadReceiptStore tx b m
       , MonadStateStore   st   m
       , Serialise         tx
       , Serialise            s
       , Crypto.Hashable   tx
       , Crypto.Hashable   st
       )
    => BlockStore tx s m
    -> Consensus tx s m
    -> Evaluator st tx b
    -> Timestamp
    -> m (Maybe (Block tx s))
mineBlock bs Consensus{cMiner} eval time = do
    txs <- (map . map) snd Mempool.getTxs
    parent <- BlockStore.getTip bs
    maybeState <- StateStore.lookupState $ blockStateHash $ blockHeader parent
    case maybeState of
        Just st -> do
            let (blockCandidate, st', receipts) = buildBlock eval time st txs (blockHash parent)
            maybeBlockHeader <- cMiner (BlockStore.getBlocks bs) (blockHeader blockCandidate)
            for maybeBlockHeader $ \header ->
                let blk = blockCandidate `withHeader` header
                 in do Mempool.delTxs (blockData blk)
                       BlockStore.insertBlock bs blk
                       StateStore.storeState st'
                       for_ receipts addReceipt
                       pure blk
        Nothing ->
            pure Nothing

-- | Mine a genesis block with the given 'Miner'.
mineGenesis
    :: (Monad m, Serialise s)
    => Miner s m
    -> Block tx r
    -> m (Either Text (Block tx s))
mineGenesis mine blk = do
    result <- mine (\_ -> pure []) (blockHeader blk)
    pure $ case result of
        Just h ->
            Right $ blk `withHeader` h
        Nothing ->
            Left $ "can't mine genesis"
