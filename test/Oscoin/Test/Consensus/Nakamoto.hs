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
import qualified Oscoin.Protocol as Protocol
import           Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Block.Pure as BlockStore.Pure
import           Oscoin.Storage.HashStore
import qualified Oscoin.Storage.Ledger as Ledger
import           Oscoin.Storage.Receipt

import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
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
    , nakBlockstore   :: BlockStore.Pure.Handle c DummyTx PoW
    , nakStateStore   :: Map (Crypto.Hashed c DummyState) DummyState
    , nakReceiptStore :: ReceiptMap c DummyTx DummyOutput
    }

deriving instance Show (Crypto.Hash c) => Show (NakamotoNodeState c)

nakStdGenL :: Lens' (NakamotoNodeState c) StdGen
nakStdGenL = lens nakStdGen (\s a -> s { nakStdGen = a })

nakMempoolL :: Lens' (NakamotoNodeState c) (Mempool.Mempool c DummyTx)
nakMempoolL = lens nakMempool (\s a -> s { nakMempool = a })

nakBlockstoreL :: Lens' (NakamotoNodeState c) (BlockStore.Pure.Handle c DummyTx PoW)
nakBlockstoreL = lens nakBlockstore (\s a -> s { nakBlockstore = a })

nakStateStoreL :: Lens' (NakamotoNodeState c) (Map (Crypto.Hashed c DummyState) DummyState)
nakStateStoreL = lens nakStateStore (\s a -> s { nakStateStore = a })

nakReceiptStoreL :: Lens' (NakamotoNodeState c) (ReceiptMap c DummyTx DummyOutput)
nakReceiptStoreL = lens nakReceiptStore (\s a -> s { nakReceiptStore = a })


---------------------------------------------------
-- * NakamotoNode
---------------------------------------------------

type NakamotoNode c = State (NakamotoNodeState c)

instance (IsCrypto c) => MonadMempool c DummyTx (NakamotoNode c) where
    addTx tx = do
        nakMempoolL %= Mempool.insert tx
        -- NOTE(ts) we don't validate transactions
        pure $ Right ()
    getTxs     = Mempool.toList <$> use nakMempoolL
    delTxs txs = nakMempoolL %= Mempool.removeTxs txs
    numTxs     = Mempool.size <$> use nakMempoolL
    lookupTx h = Mempool.lookup h <$> use nakMempoolL
    subscribe  = panic "Oscoin.Consensus.Test.Node: `subscribe` not available for pure mempool"

instance (IsCrypto c) => TestableNode c PoW (NakamotoNode c) (NakamotoNodeState c) where
    testableTick tn = do
        let blockStore = testableBlockStore
        let stateStore = mkStateHashStore nakStateStoreL
        let receiptStore = mkStateReceiptStore nakReceiptStoreL
        let ledger = Ledger.mkLedger (fst blockStore) stateStore dummyEvalBlock receiptStore
        maybeBlock <- mineBlock ledger nakConsensus tn someBeneficiary
        -- NOTE (adn): We are bypassing the protocol at the moment, but we
        -- probably shouldn't.
        forM maybeBlock $ \blk -> do
            insertBlock (snd blockStore) blk
            pure blk

    testableInit = initNakamoto
    testableRun = flip runState
    testableBlockStore = BlockStore.Pure.mkStateBlockStore nakBlockstoreL
    testableProtocol =
        -- NOTE (adn): We are bypassing the protocol at the moment, but we
        -- shouldn't. We ought to fix that in the future.
        let dispatchSync blk = do
              let (_,bs) = testableBlockStore
              Abstract.insertBlock bs blk
        in Protocol.Handle { dispatchBlockSync = dispatchSync
                           , dispatchBlockAsync = dispatchSync
                           , isNovelBlock = \h -> do
                               let (bs,_) = testableBlockStore
                               Abstract.isNovelBlock bs h
                           }
    testableBestChain nodeState =
        let blockStore = nodeState ^. nakBlockstoreL
         in unsafeToBlockchain $ BlockStore.Pure.getBlocks 10000 blockStore
         -- XXX(alexis): Don't use a magic number.


initNakamoto :: (IsCrypto c) => DummyNodeId -> NakamotoNodeState c
initNakamoto nid = NakamotoNodeState
    { nakStdGen       = mkStdGen (Hashable.hash nid)
    , nakMempool      = mempty
    , nakBlockstore   = BlockStore.Pure.genesisBlockStore genBlk Nakamoto.blockScore
    , nakStateStore   = Map.singleton (Crypto.hash genesisState) genesisState
    , nakReceiptStore = Map.empty
    }
  where
    genBlk = someGenesisBlock' emptyPoW $ Crypto.fromHashed $ Crypto.hash genesisState
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
