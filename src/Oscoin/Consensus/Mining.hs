module Oscoin.Consensus.Mining
    ( mineBlock
    , mineGenesis
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Block.Abstract (BlockStoreReader)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.HashStore
import           Oscoin.Time.Chrono (toNewestFirst)

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval (Evaluator, Receipt, buildBlock)
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool

import           Codec.Serialise (Serialise)
import           Control.Monad.Trans.Maybe
import qualified Crypto.Data.Auth.Tree.Class as AuthTree

-- | Mine a block with the given 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadMempool      c tx   m
       , Serialise         tx
       , Crypto.Hashable   c tx
       , Crypto.Hashable   c st
       , Crypto.Hashable   c (BlockHeader c Unsealed)
       , AuthTree.MerkleHash (Crypto.Hash c)
       )
    => BlockStoreReader c tx s m
    -> HashStore c st m
    -- ^ State store
    -> Consensus c tx s m
    -> Evaluator st tx o
    -> Timestamp
    -> m (Maybe (Block c tx (Sealed c s), st, [Receipt c tx o]))
mineBlock bs stateStore Consensus{cMiner} eval time = do
    txs <- (map . map) snd Mempool.getTxs
    parent <- BlockStore.getTip bs
    runMaybeT $ do
        let parentStateHash = Crypto.toHashed $ blockStateHash $ blockHeader parent
        st <- MaybeT $ lookupHashContent stateStore parentStateHash
        let (blockCandidate, st', receipts) = buildBlock eval time st txs (blockHash parent)
        sealedBlock <- MaybeT $ cMiner (map toNewestFirst . BlockStore.getBlocksByDepth bs) blockCandidate
        lift $ do
            Mempool.delTxs (blockData sealedBlock)
            pure $ (sealedBlock, st', receipts)

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
            Right sealedBlock
        Nothing ->
            Left "can't mine genesis"
