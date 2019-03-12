{-# LANGUAGE UndecidableInstances #-}
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
    , LiftTestNodeT(..)
    , withTestBlockStore
    ) where

import           Oscoin.Prelude hiding (StateT, evalStateT, runStateT, show)

import qualified Oscoin.Consensus.Nakamoto as Nakamoto (blockScore)
import           Oscoin.Crypto.Blockchain.Block
                 (Block, BlockHash, Sealed, Unsealed)
import qualified Oscoin.Crypto.Blockchain.Block as Block
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (HasHashing, Hash, Hashable(..))
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Mempool.Internal as Mempool
import qualified Oscoin.Node.Tree as STree
import           Oscoin.Storage.Block.Abstract (BlockStore(..), hoistBlockStore)
import qualified Oscoin.Storage.Block.Pure as BlockStore.Pure
import           Oscoin.Storage.Receipt (MonadReceiptStore)
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore
import           Oscoin.Storage.State.Class (MonadStateStore(..))
import           Oscoin.Test.Crypto
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Control.Monad.State.Strict
import           Data.Binary (Binary)
import qualified Data.Hashable as Hashable
import           Lens.Micro
import           Text.Show (Show(..))

import           Test.QuickCheck

-- Type class which serves as an evidence that we can hoist a 'TestNodeT'
-- computation into @m@.
class Monad m => LiftTestNodeT c s m | m -> s where
    liftTestNodeT :: TestNodeT c s Identity a -> m a


-- | Runs the given action against a test 'BlockStore' which is baked by
-- a pure, in-memory 'Handle', kept around as part of the 'NodeTestState'.
withTestBlockStore
    :: ( LiftTestNodeT c s n
       , Ord (BlockHash c)
       , Hashable c Word8
       )
    => (BlockStore c DummyTx s n -> n b)
    -> n b
withTestBlockStore action =
    let store = BlockStore
          { scoreBlock      = Nakamoto.blockScore
          , insertBlock     = \b ->
              modify (\old -> old { tnsBlockstore = BlockStore.Pure.insert b (tnsBlockstore old) })
          , getGenesisBlock =
              gets (BlockStore.Pure.getGenesisBlock . tnsBlockstore)
          , lookupBlock     = \h ->
              BlockStore.Pure.lookupBlock h <$> gets tnsBlockstore
          , lookupTx        = \tx ->
              gets (BlockStore.Pure.lookupTx tx . tnsBlockstore)
          , getOrphans      = gets (BlockStore.Pure.orphans . tnsBlockstore)
          , getBlocks       = \d ->
              gets (BlockStore.Pure.getBlocks d . tnsBlockstore)
          , getTip          = gets (BlockStore.Pure.getTip . tnsBlockstore)
          }
    in action (hoistBlockStore liftTestNodeT store)


newtype DummyTx = DummyTx Word8
    deriving (Eq, Ord, Hashable.Hashable, Binary, Serialise)

deriving instance (HasHashing c, Hashable c Word8) => Hashable c DummyTx

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
    deriving (Eq, Ord, Num, Hashable.Hashable, Binary)

deriving instance (HasHashing c, Hashable c Word8) => Hashable c DummyNodeId

instance Show DummyNodeId where
    show (DummyNodeId x) = show x

instance Arbitrary DummyNodeId where
    arbitrary = DummyNodeId <$> arbitrary

data TestNodeState c s = TestNodeState
    { tnsNodeId       :: DummyNodeId
    , tnsMempool      :: Mempool.Mempool c DummyTx
    , tnsStateStore   :: StateStore.StateStore c DummyState
    , tnsBlockstore   :: BlockStore.Pure.Handle c DummyTx s
    , tnsReceiptStore :: ReceiptStore.Store c DummyTx DummyOutput
    }

deriving instance Show (Hash c) => Show (TestNodeState c s)

class HasTestNodeState c s a | a -> s where
    testNodeStateL :: Lens' a (TestNodeState c s)

instance ReceiptStore.HasStore c DummyTx DummyOutput (TestNodeState c s) where
    storeL = lens tnsReceiptStore (\s tnsReceiptStore -> s { tnsReceiptStore })

tnsMempoolL :: Lens' (TestNodeState c s) (Mempool.Mempool c DummyTx)
tnsMempoolL = lens tnsMempool (\s a -> s { tnsMempool = a })

tnsBlockstoreL :: Lens' (TestNodeState c s) (BlockStore.Pure.Handle c DummyTx s)
tnsBlockstoreL = lens tnsBlockstore (\s a -> s { tnsBlockstore = a })

emptyTestNodeState
    :: IsCrypto c
    => (Block c DummyTx Unsealed -> Block c DummyTx (Sealed c s))
    -> DummyNodeId
    -> TestNodeState c s
emptyTestNodeState seal nid = TestNodeState
    { tnsStateStore   = StateStore.fromState genState
    , tnsMempool      = mempty
    , tnsBlockstore   = BlockStore.Pure.genesisBlockStore genBlk Nakamoto.blockScore
    , tnsNodeId       = nid
    , tnsReceiptStore = ReceiptStore.emptyStore
    }
  where
    genBlk   = seal $ Block.emptyGenesisFromState epoch genState
    genState = mempty :: DummyState

newtype TestNodeT c s m a = TestNodeT (StateT (TestNodeState c s) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , MonadState (TestNodeState c s)
             )

instance ( Ord (Hash c)
         , Hashable c Word8
         , Monad m
         ) => MonadMempool c DummyTx (TestNodeT c s m) where
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

instance ( Ord (Hash c)
         , Monad m
         ) => MonadReceiptStore c DummyTx DummyOutput (TestNodeT c s m) where
    addReceipt = ReceiptStore.addWithStore
    lookupReceipt = ReceiptStore.lookupWithStore

instance ( Monad m
         , IsCrypto c
         ) => MonadStateStore c DummyState (TestNodeT c s m) where
    lookupState k = StateStore.lookupState k <$> gets tnsStateStore
    storeState s =
        modify' $ \tn@TestNodeState{..} ->
            tn { tnsStateStore = StateStore.storeState s tnsStateStore }

    {-# INLINE lookupState #-}

runTestNodeT :: TestNodeState c s -> TestNodeT c s m a -> m (a, TestNodeState c s)
runTestNodeT s (TestNodeT ma) = runStateT ma s
