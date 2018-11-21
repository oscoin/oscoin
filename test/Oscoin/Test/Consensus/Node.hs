module Oscoin.Test.Consensus.Node
    ( DummyNodeId
    , DummyTx(..)
    , DummyState
    , DummySeal
    , dummyEval

    , TestNodeState(..)
    , TestNodeT
    , HasTestNodeState(..)
    , tnsBlockstoreL
    , emptyTestNodeState
    , runTestNodeT
    ) where

import           Oscoin.Prelude hiding (StateT, runStateT, show)

import           Oscoin.Consensus.Class (MonadQuery(..))
import           Oscoin.Crypto.Blockchain.Block (Block, StateHash)
import qualified Oscoin.Crypto.Blockchain.Block as Block
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hashable(..))
import           Oscoin.Data.Query (query)
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Mempool.Internal as Mempool
import qualified Oscoin.State.Tree as STree
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Storage.Block.Class (MonadBlockStore(..))
import           Oscoin.Storage.Receipt (MonadReceiptStore)
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore
import           Oscoin.Storage.State.Class (MonadStateStore(..))
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Control.Monad.State.Strict
import           Data.Binary (Binary)
import qualified Data.Hashable as Hashable
import           Lens.Micro
import           Text.Show (Show(..))

import           Test.QuickCheck

newtype DummyTx = DummyTx Word8
    deriving (Eq, Ord, Hashable.Hashable, Hashable, Binary, Serialise)

type DummyState = STree.Tree

type DummyOutput = ()

type DummySeal = Text


dummyEval :: Evaluator DummyState DummyTx DummyOutput
dummyEval _ s = Right ((), s)

instance Show DummyTx where
    show (DummyTx x) = show x

instance Arbitrary DummyTx where
    arbitrary = DummyTx <$> arbitrary

newtype DummyNodeId = DummyNodeId Word8
    deriving (Eq, Ord, Num, Hashable.Hashable, Hashable, Binary)

instance Show DummyNodeId where
    show (DummyNodeId x) = show x

instance Arbitrary DummyNodeId where
    arbitrary = DummyNodeId <$> arbitrary

data TestNodeState s = TestNodeState
    { tnsNodeId       :: DummyNodeId
    , tnsMempool      :: Mempool.Mempool DummyTx
    , tnsStateStore   :: StateStore.StateStore DummyState
    , tnsBlockstore   :: BlockStore.BlockStore DummyTx s
    , tnsReceiptStore :: ReceiptStore.Store DummyTx DummyOutput
    } deriving (Show)

class HasTestNodeState s a | a -> s where
    testNodeStateL :: Lens' a (TestNodeState s)

instance ReceiptStore.HasStore DummyTx DummyOutput (TestNodeState s) where
    storeL = lens tnsReceiptStore (\s tnsReceiptStore -> s { tnsReceiptStore })

tnsMempoolL :: Lens' (TestNodeState s) (Mempool.Mempool DummyTx)
tnsMempoolL = lens tnsMempool (\s a -> s { tnsMempool = a })

tnsBlockstoreL :: Lens' (TestNodeState s) (BlockStore.BlockStore DummyTx s)
tnsBlockstoreL = lens tnsBlockstore (\s a -> s { tnsBlockstore = a })

emptyTestNodeState
    :: Serialise s => (Block DummyTx () -> Block DummyTx s) -> DummyNodeId -> TestNodeState s
emptyTestNodeState seal nid = TestNodeState
    { tnsStateStore   = StateStore.fromState genState
    , tnsMempool      = mempty
    , tnsBlockstore   = BlockStore.genesisBlockStore genBlk
    , tnsNodeId       = nid
    , tnsReceiptStore = ReceiptStore.emptyStore
    }
  where
    genBlk   = seal $ Block.emptyGenesisFromState epoch genState
    genState = mempty :: DummyState

newtype TestNodeT s m a = TestNodeT (StateT (TestNodeState s) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , MonadState (TestNodeState s)
             )

instance Monad m => MonadMempool DummyTx (TestNodeT s m) where
    addTxs txs = modify' (over tnsMempoolL (Mempool.insertMany txs))
    getTxs     = Mempool.toList <$> gets tnsMempool
    delTxs txs = modify' (over tnsMempoolL (Mempool.removeTxs txs))
    numTxs     = Mempool.size <$> gets tnsMempool
    lookupTx h = Mempool.lookup h <$> gets tnsMempool
    subscribe  = panic "Oscoin.Consensus.Test.Node: `subscribe` not available for pure mempool"

    {-# INLINE addTxs #-}
    {-# INLINE getTxs #-}
    {-# INLINE delTxs #-}
    {-# INLINE numTxs #-}

instance (Serialise s, Ord s, Monad m) => MonadBlockStore DummyTx s (TestNodeT s m) where
    storeBlock  blk  = modify' (over tnsBlockstoreL (BlockStore.insert blk))
    lookupBlock hdr  = BlockStore.lookupBlock hdr <$> gets tnsBlockstore
    lookupTx    txh  = BlockStore.lookupTx txh <$> gets tnsBlockstore
    getGenesisBlock  = BlockStore.getGenesisBlock <$> gets tnsBlockstore
    getOrphans       = BlockStore.orphans <$> gets tnsBlockstore
    getBlocks d      = BlockStore.getBlocks d <$> gets tnsBlockstore
    getTip           = BlockStore.getTip <$> gets tnsBlockstore

    {-# INLINE storeBlock     #-}
    {-# INLINE lookupBlock    #-}
    {-# INLINE getOrphans     #-}
    {-# INLINE getBlocks      #-}

instance Monad m => MonadQuery (TestNodeT s m) where
    type Key (TestNodeT s m) = (StateHash, STree.Path)
    type Val (TestNodeT s m) = STree.Val

    -- TODO(alexis): This is the same code as in 'Oscoin.Node'.
    queryM (sh, k) = do
        result <- lookupState sh
        pure $ query k =<< result

    {-# INLINE queryM #-}

instance Monad m => MonadReceiptStore DummyTx DummyOutput (TestNodeT s m) where
    addReceipt = ReceiptStore.addWithStore
    lookupReceipt = ReceiptStore.lookupWithStore

instance Monad m => MonadStateStore DummyState (TestNodeT s m) where
    lookupState k = StateStore.lookupState k <$> gets tnsStateStore
    storeState s =
        modify' $ \tn@TestNodeState{..} ->
            tn { tnsStateStore = StateStore.storeState s tnsStateStore }

    {-# INLINE lookupState #-}

runTestNodeT :: TestNodeState s -> TestNodeT s m a -> m (a, TestNodeState s)
runTestNodeT s (TestNodeT ma) = runStateT ma s
