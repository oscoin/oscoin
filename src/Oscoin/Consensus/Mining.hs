module Oscoin.Consensus.Mining
    ( mineBlock
    , mineGenesis
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Block.Class (MonadBlockStore)
import qualified Oscoin.Storage.Block.Class as BlockStore

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval (Evaluator, buildBlock)
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Storage.Receipt.Class

import           Codec.Serialise (Serialise)

-- | Mine a block with the given 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       , MonadReceiptStore tx b m
       , Serialise       tx
       , Crypto.Hashable tx
       )
    => Consensus tx m
    -> Evaluator s tx b
    -> Timestamp
    -> m (Maybe (Block tx s))
mineBlock Consensus{cScore, cMiner} eval time = do
    txs   <- (map . map) snd Mempool.getTxs
    chain <- BlockStore.maximumChainBy cScore
    let parent = tip chain
    let (blockCandidate, receipts) = buildBlock eval time txs parent
    maybeBlockHeader <- cMiner (Just chain) (blockHeader blockCandidate)
    for maybeBlockHeader $ \header -> do
        let blk = blockCandidate { blockHeader = header }
        Mempool.delTxs (blockData blk)
        BlockStore.storeBlock $ map (const . Just) blk
        for_ receipts addReceipt
        pure blk

-- | Mine a genesis block with the given 'Miner'.
mineGenesis
    :: (Monad m)
    => Miner m
    -> Block tx s
    -> m (Either Text (Block tx s))
mineGenesis mine blk = do
    result <- mine Nothing (blockHeader blk)
    pure $ case result of
        Just h ->
            Right $ blk { blockHeader = h }
        Nothing ->
            Left $ "can't mine genesis"
