module Oscoin.Consensus.Mining
    ( mineBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Ledger
import           Oscoin.Time.Chrono (toNewestFirst)

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (Beneficiary)
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
       , Serialise         (Beneficiary c)
       , Crypto.Hashable   c tx
       , Crypto.Hashable   c st
       , Crypto.Hashable   c (BlockHeader c Unsealed)
       , AuthTree.MerkleHash (Crypto.Hash c)
       )
    => Ledger c s tx o st m
    -> Consensus c tx s m
    -> Timestamp
    -> Beneficiary c
    -> m (Maybe (Block c tx (Sealed c s)))
mineBlock ledger Consensus{cMiner} time benef = do
    txs <- (map . map) snd Mempool.getTxs
    unsealedBlock_ <- buildNextBlock ledger time benef txs
    case unsealedBlock_ of
        Left _err -> pure $ Nothing
        Right unsealedBlock -> runMaybeT $ do
            sealedBlock <- MaybeT $ cMiner (map toNewestFirst . getBlocksByDepth ledger) unsealedBlock
            lift $ do
                Mempool.delTxs (blockTxs sealedBlock)
                pure $ sealedBlock
