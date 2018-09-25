module Oscoin.Consensus.Mining
    ( mineBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.Consensus.Evaluator (Evaluator, applyValidExprs)
import           Oscoin.Consensus.Types

import           Oscoin.Clock (Tick)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool

import           Codec.Serialise (Serialise)

-- | Mine a block with the given 'Consensus' on top of the best chain obtained from
-- 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       , Serialise       tx
       )
    => Consensus tx m
    -> Evaluator s tx b
    -> Tick
    -> m (Maybe (Block tx s))
mineBlock Consensus{..} eval tick = do
    txs    <- (map . map) snd Mempool.getTxs
    parent <- tip <$> BlockStore.maximumChainBy cScore
    let (results, newState) = applyValidExprs txs (blockState . blockHeader $ parent) eval
        validTxs = map fst (rights results)
        headerCandidate = BlockHeader
            { blockPrevHash     = blockHash parent
            , blockDataHash     = hashTxs validTxs
            , blockState        = newState
            , blockDifficulty   = 0
            , blockTimestamp    = fromIntegral $ fromEnum tick
            , blockNonce        = 0
            }

    maybeBlockHeader <- cMiner headerCandidate
    for maybeBlockHeader $ \header -> do
        let blk = mkBlock header txs
        Mempool.delTxs (blockData blk)
        BlockStore.storeBlock $ map (const . Just) blk
        pure blk