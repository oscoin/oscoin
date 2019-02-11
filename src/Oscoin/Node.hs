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
                 , StateHash
                 , blockHash
                 )
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hashable, Hashed, formatHash)
import           Oscoin.Data.Query
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

import qualified Radicle.Extended as Rad

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Lens.Micro ((^.))

withNode
    :: Config
    -> i
    -> Mempool.Handle tx
    -> StateStore.Handle st
    -> BlockStore tx s IO
    -> Evaluator st tx Rad.Value
    -> Consensus tx s (NodeT tx st s i IO)
    -> (Handle tx st s i -> IO c)
    -> IO c
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
    :: ( MonadIO            m
       , P2P.MonadBroadcast m
       , MonadClock         m
       , Serialise tx
       , Hashable  tx
       , Serialise s
       , Hashable  st
       )
    => NodeT tx st s i m a
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
       , Serialise s
       , Hashable tx
       , Hashable st
       )
    => NodeT tx st s i m (Maybe (Block tx s))
mineBlock = do
    Handle{hEval, hConsensus, hBlockStore} <- ask
    time <- currentTick
    hoist liftIO $ Consensus.mineBlock (hoistBlockStore lift hBlockStore) hConsensus hEval time

storage
    :: ( MonadIO m
       , Hashable  tx
       , Hashable  st
       , Serialise tx
       , Serialise s
       )
    => Evaluator st tx o
    -> Validate tx s
    -> Consensus.Config
    -> Storage tx s (NodeT tx st s i m)
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

getMempool :: MonadIO m => NodeT tx st s i m (Mempool tx)
getMempool = asks hMempool >>= liftIO . atomically . Mempool.snapshot

-- | Get a value from the given state hash.
getPath :: (Query st, Hashable st, MonadIO m) => StateHash -> STree.Path -> NodeT tx st s i m (Maybe (QueryVal st))
getPath sh p = queryM (sh, p)

-- | Get a value from the latest state.
getPathLatest
    :: (MonadIO m, Query st, Hashable st)
    => STree.Path
    -> NodeT tx st s i m (Maybe (StateHash, QueryVal st))
getPathLatest path = do
    bs <- asks hBlockStore
    stateHash <- blockStateHash . blockHeader <$> BlockStore.getTip (hoistBlockStore liftIO bs)
    result <- getPath stateHash path
    for result $ \v ->
        pure (stateHash, v)

getBlocks :: (MonadIO m) => Depth -> NodeT tx st s i m [Block tx s]
getBlocks d = withBlockStore (`BlockStore.getBlocks` d)

lookupTx :: (MonadIO m) => Hashed tx -> NodeT tx st s i m (Maybe (TxLookup tx))
lookupTx tx = withBlockStore (`BlockStore.lookupTx` tx)

lookupBlock :: (MonadIO m) => BlockHash -> NodeT tx st s i m (Maybe (Block tx s))
lookupBlock h = withBlockStore (`BlockStore.lookupBlock` h)
