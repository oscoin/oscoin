module Oscoin.Test.Consensus.Node
    ( DummyNodeId
    , DummyTx(..)
    , DummyState
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
import qualified Oscoin.Crypto.Blockchain.Block as Block
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hashable(..))
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Mempool.Internal as Mempool
import qualified Oscoin.State.Tree as STree
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Storage.Block.Class (MonadBlockStore(..))
import           Oscoin.Storage.Receipt (MonadReceiptStore)
import qualified Oscoin.Storage.Receipt as ReceiptStore
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

type DummyState = ()

type DummyOutput = ()


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

data TestNodeState = TestNodeState
    { tnsStateTree    :: STree.Tree STree.Path STree.Val
    , tnsMempool      :: Mempool.Mempool DummyTx
    , tnsBlockstore   :: BlockStore.BlockStore DummyTx ()
    , tnsNodeId       :: DummyNodeId
    , tnsReceiptStore :: ReceiptStore.Store DummyTx DummyOutput
    } deriving (Show)

class HasTestNodeState a where
    testNodeStateL :: Lens' a TestNodeState

instance ReceiptStore.HasStore DummyTx DummyOutput TestNodeState where
    storeL = lens tnsReceiptStore (\s tnsReceiptStore -> s { tnsReceiptStore })

tnsMempoolL :: Lens' TestNodeState (Mempool.Mempool DummyTx)
tnsMempoolL = lens tnsMempool (\s a -> s { tnsMempool = a })

tnsBlockstoreL :: Lens' TestNodeState (BlockStore.BlockStore DummyTx ())
tnsBlockstoreL = lens tnsBlockstore (\s a -> s { tnsBlockstore = a })

emptyTestNodeState :: DummyNodeId -> TestNodeState
emptyTestNodeState nid = TestNodeState
    { tnsStateTree  = mempty
    , tnsMempool    = mempty
    , tnsBlockstore = BlockStore.genesisBlockStore $ Block.emptyGenesisBlock epoch ()
    , tnsNodeId     = nid
    , tnsReceiptStore = ReceiptStore.emptyStore
    }

newtype TestNodeT m a = TestNodeT (StateT TestNodeState m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , MonadState TestNodeState
             )

instance Monad m => MonadMempool DummyTx (TestNodeT m) where
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

instance Monad m => MonadBlockStore DummyTx () (TestNodeT m) where
    storeBlock  blk  = modify' (over tnsBlockstoreL (BlockStore.insert blk))
    lookupBlock hdr  = BlockStore.lookupBlock hdr <$> gets tnsBlockstore
    lookupTx    txh  = BlockStore.lookupTx txh <$> gets tnsBlockstore
    getGenesisBlock  = BlockStore.getGenesisBlock <$> gets tnsBlockstore
    orphans          = BlockStore.orphans <$> gets tnsBlockstore
    maximumChainBy f = BlockStore.maximumChainBy f <$> gets tnsBlockstore

    {-# INLINE storeBlock     #-}
    {-# INLINE lookupBlock    #-}
    {-# INLINE orphans        #-}
    {-# INLINE maximumChainBy #-}

instance Monad m => MonadQuery (TestNodeT m) where
    type Key (TestNodeT m) = STree.Path
    type Val (TestNodeT m) = STree.Val

    queryM k = STree.get k <$> gets tnsStateTree

    {-# INLINE queryM #-}

instance Monad m => MonadReceiptStore DummyTx DummyOutput (TestNodeT m) where
    addReceipt = ReceiptStore.addWithStore
    lookupReceipt = ReceiptStore.lookupWithStore

runTestNodeT :: TestNodeState -> TestNodeT m a -> m (a, TestNodeState)
runTestNodeT s (TestNodeT ma) = runStateT ma s
