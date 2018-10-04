module Oscoin.Consensus.Mining
    ( mineBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.Consensus.Evaluator (Evaluator, applyValidExprs)
import           Oscoin.Consensus.Types

import           Oscoin.Crypto.Blockchain
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool

import           Codec.Serialise (Serialise)

-- | Mine a block with the given 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       , Serialise       tx
       )
    => Consensus tx m
    -> Evaluator s tx b
    -> Timestamp
    -> m (Maybe (Block tx s))
mineBlock Consensus{cScore, cMiner} eval time = do
    txs   <- (map . map) snd Mempool.getTxs
    chain <- BlockStore.maximumChainBy cScore

    let parent = tip chain

    maybeBlockHeader <- cMiner chain (headerCandidate txs parent)
    for maybeBlockHeader $ \header -> do
        let blk = mkBlock header txs
        Mempool.delTxs (blockData blk)
        BlockStore.storeBlock $ map (const . Just) blk
        pure blk
  where
    headerCandidate txs parent =
        let (validTxs, newState) = first (map fst . rights)
                                 $ applyValidExprs txs
                                                   (blockState . blockHeader $ parent)
                                                   eval
         in BlockHeader
            { blockPrevHash     = blockHash parent
            , blockDataHash     = hashTxs validTxs
            , blockState        = newState
            , blockDifficulty   = 0
            , blockTimestamp    = time
            , blockNonce        = 0
            }
