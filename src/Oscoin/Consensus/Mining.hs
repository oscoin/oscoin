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
import qualified Crypto.Data.Auth.Tree.Internal as AuthTree

-- | Mine a block with the given 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadMempool      c tx   m
       , MonadReceiptStore c tx b m
       , MonadStateStore   c st   m
       , Serialise         tx
       , Crypto.Hashable   c tx
       , Crypto.Hashable   c st
       , Crypto.Hashable   c (BlockHeader c Unsealed)
       , AuthTree.MerkleHash (Crypto.Hash c)
       )
    => BlockStore c tx s m
    -> Consensus c tx s m
    -> Evaluator st tx b
    -> Timestamp
    -> m (Maybe (Block c tx (Sealed c s)))
mineBlock bs Consensus{cMiner} eval time = do
    txs <- (map . map) snd Mempool.getTxs
    parent <- BlockStore.getTip bs
    maybeState <- StateStore.lookupState $ blockStateHash $ blockHeader parent
    case maybeState of
        Just st -> do
            let (blockCandidate, st', receipts) = buildBlock eval time st txs (blockHash parent)
            maybeSealedBlock <- cMiner (BlockStore.getBlocks bs) blockCandidate
            for maybeSealedBlock $ \blk -> do
                Mempool.delTxs (blockData blk)
                BlockStore.insertBlock bs blk
                StateStore.storeState st'
                for_ receipts addReceipt
                pure blk
        Nothing ->
            pure Nothing

-- | Mine a genesis block with the given 'Miner'.
mineGenesis
    :: Monad m
    => Miner c s m
    -> Block c tx Unsealed
    -> m (Either Text (Block c tx (Sealed c s)))
mineGenesis mine blk = do
    result <- mine (\_ -> pure []) blk
    pure $ case result of
        Just sealedBlock ->
            Right $ sealedBlock
        Nothing ->
            Left $ "can't mine genesis"
