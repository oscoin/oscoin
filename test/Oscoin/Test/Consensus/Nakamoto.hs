{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Test.Consensus.Nakamoto
    ( NakamotoNodeState
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Mining (mineBlock)
import           Oscoin.Consensus.Nakamoto (PoW(..), emptyPoW)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Mempool.Internal as Mempool
import           Oscoin.Storage.Block.Abstract
import qualified Oscoin.Storage.Block.Pure as BlockStore.Pure
import           Oscoin.Storage.HashStore
import           Oscoin.Storage.Receipt
import           Oscoin.Storage.State
import           Oscoin.Time

import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Crypto
import           Test.Oscoin.DummyLedger

import           Codec.Serialise (Serialise)
import qualified Data.Hashable as Hashable
import qualified Data.Map.Strict as Map
import           Lens.Micro
import           Lens.Micro.Mtl
import           System.Random


---------------------------------------------------
-- * NakamotoNodeState
---------------------------------------------------

data NakamotoNodeState c = NakamotoNodeState
    { nakStdGen       :: StdGen
    , nakMempool      :: Mempool.Mempool c DummyTx
    , nakReceiptStore :: ReceiptMap c DummyTx DummyOutput
    , nakBlockstore   :: BlockStore.Pure.Handle c DummyTx PoW
    , nakStateStore   :: Map (Crypto.Hashed c DummyState) DummyState
    }

deriving instance Show (Crypto.Hash c) => Show (NakamotoNodeState c)

nakStdGenL :: Lens' (NakamotoNodeState c) StdGen
nakStdGenL = lens nakStdGen (\s a -> s { nakStdGen = a })

nakMempoolL :: Lens' (NakamotoNodeState c) (Mempool.Mempool c DummyTx)
nakMempoolL = lens nakMempool (\s a -> s { nakMempool = a })

nakReceiptStoreL :: Lens' (NakamotoNodeState c) (ReceiptMap c DummyTx DummyOutput)
nakReceiptStoreL = lens nakReceiptStore (\s a -> s { nakReceiptStore = a })

nakBlockstoreL :: Lens' (NakamotoNodeState c) (BlockStore.Pure.Handle c DummyTx PoW)
nakBlockstoreL = lens nakBlockstore (\s a -> s { nakBlockstore = a })

nakStateStoreL :: Lens' (NakamotoNodeState c) (Map (Crypto.Hashed c DummyState) DummyState)
nakStateStoreL = lens nakStateStore (\s a -> s { nakStateStore = a })


---------------------------------------------------
-- * NakamotoNode
---------------------------------------------------

type NakamotoNode c = State (NakamotoNodeState c)

instance (IsCrypto c) => MonadReceiptStore c DummyTx DummyOutput (NakamotoNode c) where
    getReceiptStore = pure $ mkStateReceiptStore nakReceiptStoreL

instance (IsCrypto c) => MonadStateStore c DummyState (NakamotoNode c) where
    getStateStore = pure $ mkStateHashStore nakStateStoreL

instance (IsCrypto c) => MonadMempool c DummyTx (NakamotoNode c) where
    addTxs txs = nakMempoolL %= Mempool.insertMany txs
    getTxs     = Mempool.toList <$> use nakMempoolL
    delTxs txs = nakMempoolL %= Mempool.removeTxs txs
    numTxs     = Mempool.size <$> use nakMempoolL
    lookupTx h = Mempool.lookup h <$> use nakMempoolL
    subscribe  = panic "Oscoin.Consensus.Test.Node: `subscribe` not available for pure mempool"

instance (IsCrypto c) => TestableNode c PoW (NakamotoNode c) (NakamotoNodeState c) where
    testableTick tn = do
        let (blockStoreReader, blockStoreWriter) = testableBlockStore
        maybeBlock <- mineBlock blockStoreReader nakConsensus dummyEval tn
        -- NOTE (adn): We are bypassing the protocol at the moment, but we
        -- probably shouldn't.
        forM_ maybeBlock $ \blk -> insertBlock blockStoreWriter blk
        pure maybeBlock

    testableInit = initNakamoto
    testableRun = flip runState
    testableBlockStore = BlockStore.Pure.mkStateBlockStore nakBlockstoreL
    testableBestChain nodeState =
        let blockStore = nodeState ^. nakBlockstoreL
         in unsafeToBlockchain $ BlockStore.Pure.getBlocks 10000 blockStore
         -- XXX(alexis): Don't use a magic number.


initNakamoto :: (IsCrypto c) => DummyNodeId -> NakamotoNodeState c
initNakamoto nid = NakamotoNodeState
    { nakStdGen = mkStdGen (Hashable.hash nid)
    , nakMempool = mempty
    , nakReceiptStore = Map.empty
    , nakBlockstore   = BlockStore.Pure.genesisBlockStore genBlk Nakamoto.blockScore
    , nakStateStore   = Map.singleton (Crypto.hash genesisState) genesisState
    }
  where
    genBlk   = sealBlock emptyPoW $ emptyGenesisFromState epoch genesisState
    genesisState = mempty :: DummyState


---------------------------------------------------
-- * Consensus
---------------------------------------------------

nakConsensus :: (IsCrypto c, Serialise tx) => Consensus c tx PoW (NakamotoNode c)
nakConsensus = Consensus
    { cScore = comparing Nakamoto.chainScore
    , cMiner = \_chain blk -> zoom nakStdGenL $ do
        gen <- state split
        pure $ mineBlockRandom gen blk
    , cValidate = Nakamoto.validateFull
    }

-- | Mine a block header in 10% of the cases. The forged header does
-- not have a valid proof of work.
mineBlockRandom
    :: (IsCrypto c)
    => StdGen
    -> Block c tx Unsealed
    -> Maybe (Block c tx (Sealed c PoW))
mineBlockRandom stdGen blk =
    let r = fst $ randomR (0, 1) stdGen
        p = 0.1 :: Float
     in if r < p
           then Just $ sealBlock emptyPoW blk
           else Nothing
