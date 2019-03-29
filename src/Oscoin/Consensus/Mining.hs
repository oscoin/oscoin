module Oscoin.Consensus.Mining
    ( mineBlock
    , mineGenesis
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Ledger
import           Oscoin.Time.Chrono (toNewestFirst)

import           Oscoin.Crypto.Blockchain
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
    => Ledger c s tx o st m
    -> Consensus c tx s m
    -> Timestamp
    -> m (Maybe (Block c tx (Sealed c s)))
mineBlock ledger Consensus{cMiner} time = do
    txs <- (map . map) snd Mempool.getTxs
    unsealedBlock_ <- buildNextBlock ledger time txs
    case unsealedBlock_ of
        Left _err -> pure $ Nothing
        Right unsealedBlock -> runMaybeT $ do
            sealedBlock <- MaybeT $ cMiner (map toNewestFirst . getBlocksByDepth ledger) unsealedBlock
            lift $ do
                Mempool.delTxs (blockData sealedBlock)
                pure $ sealedBlock

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
