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
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Consensus(..), Validate)
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Consensus.Class (MonadClock(..), MonadQuery(..))
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHeader(..), Depth, StateHash, blockHash)
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hashable, formatHash)
import           Oscoin.Data.Query
import qualified Oscoin.Environment as Env
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import           Oscoin.Node.Trans
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.P2P as P2P
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import           Oscoin.Telemetry (NotableEvent(..))
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (ftag, stext, (%))
import qualified Oscoin.Telemetry.Logging as Log

import qualified Oscoin.Storage.Block.Class as BlockStore
import qualified Oscoin.Storage.Block.STM as BlockStore
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore

import qualified Radicle.Extended as Rad

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Lens.Micro ((^.))

withNode
    :: (Hashable tx)
    => Config
    -> i
    -> Mempool.Handle tx
    -> StateStore.Handle st
    -> BlockStore.Handle tx s
    -> Evaluator st tx Rad.Value
    -> Consensus tx s (NodeT tx st s i IO)
    -> (Handle tx st s i -> IO c)
    -> IO c
withNode hConfig hNodeId hMempool hStateStore hBlockStore hEval hConsensus =
    bracket open close
  where
    open = do
        hReceiptStore <- ReceiptStore.newHandle
        gen <- runNodeT Handle{..} BlockStore.getGenesisBlock
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
    Handle{hEval, hConsensus, hConfig} <- ask
    let telemetryHandle = cfgTelemetry hConfig
    forever $ do
        nTxs <- numTxs
        if nTxs == 0 && cfgNoEmptyBlocks hConfig
        then
            liftIO $ threadDelay $ 1000 * 1000
        else do
            time <- currentTick
            blk <- hoist liftIO $ Consensus.mineBlock hConsensus hEval time

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
    Handle{hEval, hConsensus} <- ask
    time <- currentTick
    hoist liftIO $ Consensus.mineBlock hConsensus hEval time

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
    { storageApplyBlock  = Storage.applyBlock eval validate config
    , storageApplyTx     = Storage.applyTx
    , storageLookupBlock = Storage.lookupBlock
    , storageLookupTx    = Storage.lookupTx
    }

getMempool :: MonadIO m => NodeT tx st s i m (Mempool tx)
getMempool = asks hMempool >>= liftIO . atomically . Mempool.snapshot

-- | Get a value from the given state hash.
getPath :: (Query st, Hashable st, MonadIO m) => StateHash -> STree.Path -> NodeT tx st s i m (Maybe (QueryVal st))
getPath sh p = queryM (sh, p)

-- | Get a value from the latest state.
getPathLatest
    :: (Hashable tx, MonadIO m, Query st, Hashable st)
    => STree.Path -> NodeT tx st s i m (Maybe (StateHash, QueryVal st))
getPathLatest path = do
    stateHash <- blockStateHash . blockHeader <$> BlockStore.getTip
    result <- getPath stateHash path
    for result $ \v ->
        pure (stateHash, v)

getBlocks :: (Hashable tx, MonadIO m) => Depth -> NodeT tx st s i m [Block tx s]
getBlocks = BlockStore.getBlocks
