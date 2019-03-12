module Oscoin.Node
    ( Config (..)
    , Handle
    , NodeT

    , withNode

    , runNodeT
    , mineBlock

    , miner
    , storage

    , getMempool
    , getPath
    , getPathLatest
    , getBlocks
    , lookupTx
    , lookupBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Consensus(..), Validate)
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Consensus.Class (MonadClock(..), MonadQuery(..))
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain (TxLookup)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Depth
                 , Sealed
                 , StateHash
                 , Unsealed
                 , blockHash
                 )
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hash, Hashable, Hashed, formatHash)
import           Oscoin.Data.Query
import qualified Oscoin.Data.RadicleTx as RadicleTx
import qualified Oscoin.Environment as Env
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Node.Trans
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.P2P as P2P
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import           Oscoin.Telemetry (NotableEvent(..))
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (ftag, stext, (%))
import qualified Oscoin.Telemetry.Logging as Log

import           Oscoin.Storage.Block.Abstract (BlockStore, hoistBlockStore)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import qualified Crypto.Data.Auth.Tree.Internal as AuthTree
import           Lens.Micro ((^.))

withNode
    :: Log.Buildable (Hash c)
    => Config
    -> i
    -> Mempool.Handle c tx
    -> StateStore.Handle c st
    -> BlockStore c tx s IO
    -> Evaluator st tx RadicleTx.Output
    -> Consensus c tx s (NodeT c tx st s i IO)
    -> (Handle c tx st s i -> IO a)
    -> IO a
withNode hConfig hNodeId hMempool hStateStore hBlockStore hEval hConsensus =
    bracket open close
  where
    open = do
        hReceiptStore <- ReceiptStore.newHandle
        gen <- liftIO (BlockStore.getGenesisBlock hBlockStore)
        Log.info (cfgTelemetry hConfig ^. Log.loggerL)
                 "running in"
                 (ftag "env" % stext) (Env.toText $ cfgEnv hConfig)
        Log.info (cfgTelemetry hConfig ^. Log.loggerL)
                 "genesis is"
                 (ftag "block_hash" % formatHash) (blockHash gen)
        pure Handle{..}

    close = const $ pure ()

miner
    :: ( MonadIO              m
       , P2P.MonadBroadcast c m
       , MonadClock           m
       , Serialise   tx
       , Hashable  c tx
       , Serialise s
       , Hashable  c st
       , Ord (Hash c)
       , Serialise (Hash c)
       , Serialise Unsealed
       , AuthTree.MerkleHash (Hash c)
       , Log.Buildable (Hash c)
       )
    => NodeT c tx st s i m a
miner = do
    Handle{hEval, hConsensus, hConfig, hBlockStore} <- ask
    let telemetryHandle = cfgTelemetry hConfig
    forever $ do
        nTxs <- Mempool.numTxs
        if nTxs == 0 && cfgNoEmptyBlocks hConfig
        then
            liftIO $ threadDelay $ 1000 * 1000
        else do
            time <- currentTick
            blk <- hoist liftIO $ Consensus.mineBlock (hoistBlockStore lift hBlockStore) hConsensus hEval time

            for_ blk $ \b -> do
                lift   $ P2P.broadcast $ P2P.BlockMsg b
                liftIO $ Telemetry.emit telemetryHandle (BlockMinedEvent (blockHash b))

-- | Mine a block with the node’s 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadIO m
       , MonadClock m
       , Serialise tx
       , Hashable c tx
       , Hashable c st
       , Ord (Hash c)
       , Serialise (Hash c)
       , Serialise Unsealed
       , AuthTree.MerkleHash (Hash c)
       )
    => NodeT c tx st s i m (Maybe (Block c tx (Sealed c s)))
mineBlock = do
    Handle{hEval, hConsensus, hBlockStore} <- ask
    time <- currentTick
    hoist liftIO $ Consensus.mineBlock (hoistBlockStore lift hBlockStore) hConsensus hEval time

storage
    :: ( MonadIO m
       , Hashable  c tx
       , Hashable  c st
       , Serialise tx
       , Serialise s
       , Serialise (Hash c)
       , Log.Buildable (Hash c)
       , Ord (StateHash c)
       )
    => Evaluator st tx o
    -> Validate c tx s
    -> Consensus.Config
    -> Storage c tx s (NodeT c tx st s i m)
storage eval validate config = Storage
    { storageApplyBlock = \blk -> do
        bs <- asks hBlockStore
        Storage.applyBlock (hoistBlockStore liftIO bs) eval validate config blk
    , storageApplyTx     = \tx -> do
        bs <- asks hBlockStore
        Storage.applyTx (hoistBlockStore liftIO bs) tx
    , storageLookupBlock = \blk -> do
        bs <- asks hBlockStore
        BlockStore.lookupBlock (hoistBlockStore liftIO bs) blk
    , storageLookupTx    = \tx -> do
        bs <- asks hBlockStore
        Storage.lookupTx (hoistBlockStore liftIO bs) tx
    }

getMempool :: MonadIO m => NodeT c tx st s i m (Mempool c tx)
getMempool = asks hMempool >>= liftIO . atomically . Mempool.snapshot

-- | Get a value from the given state hash.
getPath
    :: ( Query st
       , Hashable c st
       , MonadIO m
       , Ord (StateHash c)
       )
    => StateHash c
    -> STree.Path
    -> NodeT c tx st s i m (Maybe (QueryVal st))
getPath sh p = queryM (sh, p)

-- | Get a value from the latest state.
getPathLatest
    :: ( MonadIO m
       , Query st
       , Hashable c st
       , Ord (StateHash c)
       )
    => STree.Path
    -> NodeT c tx st s i m (Maybe (StateHash c, QueryVal st))
getPathLatest path = do
    bs <- asks hBlockStore
    stateHash <- blockStateHash . blockHeader <$> BlockStore.getTip (hoistBlockStore liftIO bs)
    result <- getPath stateHash path
    for result $ \v ->
        pure (stateHash, v)

getBlocks :: (MonadIO m) => Depth -> NodeT c tx st s i m [Block c tx (Sealed c s)]
getBlocks d = withBlockStore (`BlockStore.getBlocks` d)

lookupTx :: (MonadIO m) => Hashed c tx -> NodeT c tx st s i m (Maybe (TxLookup c tx))
lookupTx tx = withBlockStore (`BlockStore.lookupTx` tx)

lookupBlock
    :: (MonadIO m)
    => BlockHash c
    -> NodeT c tx st s i m (Maybe (Block c tx (Sealed c s)))
lookupBlock h = withBlockStore (`BlockStore.lookupBlock` h)
