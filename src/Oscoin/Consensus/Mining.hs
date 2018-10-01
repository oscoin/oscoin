module Oscoin.Consensus.Mining
    ( mineBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.Consensus.Evaluator
import           Oscoin.Consensus.Types
import qualified Oscoin.Crypto.Hash as Crypto

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval (buildBlock)
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool

import           Codec.Serialise (Serialise)

-- | Mine a block with the given 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
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
    let (blockCandidate, _) = buildBlock eval time txs parent
    maybeBlockHeader <- cMiner chain (blockHeader blockCandidate)
    for maybeBlockHeader $ \header -> do
        let blk = blockCandidate { blockHeader = header }
        Mempool.delTxs (blockData blk)
        BlockStore.storeBlock $ map (const . Just) blk
        pure blk
