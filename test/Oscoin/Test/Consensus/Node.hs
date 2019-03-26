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
    , stepTestProtocol
    ) where

import           Oscoin.Prelude hiding (StateT, evalStateT, runStateT, show)

import qualified Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto (blockScore)
import           Oscoin.Consensus.Types (Validate)
import           Oscoin.Crypto.Blockchain.Block
                 (Block, BlockHash, Score, Sealed, Unsealed)
import qualified Oscoin.Crypto.Blockchain.Block as Block
import           Oscoin.Crypto.Hash (HasHashing, Hash, Hashable(..))
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Mempool.Internal as Mempool
import           Oscoin.Protocol (protoOrphanage, stepProtocol, withProtocol)
import           Oscoin.Storage.Block.Abstract
                 ( BlockStore
                 , BlockStoreReader(..)
                 , BlockStoreWriter(..)
                 , hoistBlockStoreReader
                 , hoistBlockStoreWriter
                 )
import           Oscoin.Storage.Block.Orphanage (Orphanage, emptyOrphanage)
import qualified Oscoin.Storage.Block.Pure as BlockStore.Pure
import           Oscoin.Storage.Receipt (MonadReceiptStore)
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore
import           Oscoin.Storage.State.Class (MonadStateStore(..))
import           Oscoin.Telemetry.Logging (Buildable)
import           Oscoin.Test.Crypto
import           Oscoin.Time
import           Oscoin.Time.Chrono as Chrono

import           Control.Monad.State.Strict
import           Data.Binary (Binary)
import qualified Data.Hashable as Hashable
import           Lens.Micro
import           Text.Show (Show(..))

import           Test.Oscoin.DummyLedger
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
    let publicAPI = BlockStoreReader
            { getGenesisBlock =
                gets (BlockStore.Pure.getGenesisBlock . tnsBlockstore)
            , lookupBlock     = \h ->
                BlockStore.Pure.lookupBlock h <$> gets tnsBlockstore
            , lookupTx        = \tx ->
                gets (BlockStore.Pure.lookupTx tx . tnsBlockstore)
            -- The pure store doesn't guarantee block ordering as 'getBlocks'
            -- sorts using the score function.
            , getBlocksByDepth = \d ->
                  Chrono.NewestFirst <$> gets (BlockStore.Pure.getBlocks d . tnsBlockstore)
            , getBlocksByParentHash = \h ->
                  Chrono.NewestFirst <$> gets (BlockStore.Pure.getChainSuffix h . tnsBlockstore)
            , getTip          = gets (BlockStore.Pure.getTip . tnsBlockstore)
            }
        privateAPI = BlockStoreWriter
            { insertBlock     = \b ->
                modify' (\old -> old { tnsBlockstore = BlockStore.Pure.insert b (tnsBlockstore old) })
            , switchToFork = \_ _ -> pure ()
            }
    in action ( hoistBlockStoreReader liftTestNodeT publicAPI
              , hoistBlockStoreWriter liftTestNodeT privateAPI
              )

-- | Internally builds a 'Protocol' and steps it, making sure the state is
-- persisted in the underlying monad.
-- a pure, in-memory 'BlockStore', kept around as part of the 'NodeTestState'.
stepTestProtocol
    :: ( LiftTestNodeT c s n
       , Ord (BlockHash c)
       , Hashable c Word8
       , Ord s
       , Buildable (Hash c)
       )
    => BlockStore c DummyTx s n
    -> n (Maybe (Block c DummyTx (Sealed c s)))
    -- ^ An action to fetch the next block, if any.
    -> Validate c DummyTx s
    -> (Block c DummyTx (Sealed c s) -> Score)
    -> n ()
stepTestProtocol fullBlockStore fetchNextBlock validateFull scoreBlock = do
   -- The Consensus config is hardcoded for now, but it's fairly easy
   -- to pass externally.
   let config = Consensus.Config 1024 10

   -- Execute the action and step the protocol
   o              <- liftTestNodeT $ gets tnsOrphanage
   mbBlock        <- fetchNextBlock
   case mbBlock of
     Nothing -> pure ()
     Just blk -> do
         (protocol', _) <- withProtocol o validateFull scoreBlock fullBlockStore config $ \protocol ->
             stepProtocol protocol blk
         liftTestNodeT $ modify' $ \st -> st { tnsOrphanage = protoOrphanage protocol' }


type DummySeal = Text

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
    , tnsOrphanage    :: Orphanage c DummyTx s
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
    , tnsOrphanage    = emptyOrphanage Nakamoto.blockScore
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
