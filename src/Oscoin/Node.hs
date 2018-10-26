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
    , getBestChain
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Consensus(..))
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Consensus.Class
                 (MonadClock(..), MonadQuery(..), MonadUpdate(..))
import           Oscoin.Crypto.Blockchain (Blockchain)
import           Oscoin.Crypto.Blockchain.Block (Block, blockHash)
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hashable, formatHashed)
import           Oscoin.Data.Query
import           Oscoin.Logging ((%))
import qualified Oscoin.Logging as Log
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import           Oscoin.Node.Trans
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.P2P as P2P
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import qualified Oscoin.Storage.Block as BlockStore


import           Oscoin.Storage.Block.Class
                 (MonadBlockStore(..), chainState, maximumChainBy)
import qualified Oscoin.Storage.Block.STM as BlockStore
import qualified Oscoin.Storage.Receipt as ReceiptStore

import qualified Radicle.Extended as Rad

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))

withNode
    :: Config
    -> i
    -> Mempool.Handle tx
    -> STree.Handle s
    -> BlockStore.Handle tx s
    -> Evaluator s tx Rad.Value
    -> Consensus tx (NodeT tx s i IO)
    -> (Handle tx s i -> IO c)
    -> IO c
withNode hConfig hNodeId hMempool hStateTree hBlockStore hEval hConsensus =
    bracket open close
  where
    open = do
        hReceiptStore <- ReceiptStore.newHandle
        gen <- atomically $ BlockStore.for hBlockStore $ \bs ->
            BlockStore.getGenesisBlock bs
        Log.info (cfgLogger hConfig) ("genesis is " % formatHashed) (blockHash gen)
        pure Handle{..}

    close = const $ pure ()

miner
    :: ( MonadIO            m
       , P2P.MonadBroadcast m
       , MonadClock         m
       , Serialise tx
       , Hashable  tx
       , Ord       tx
       )
    => NodeT tx s i m a
miner = do
    Handle{hEval, hConsensus} <- ask
    forever $ do
        nTxs <- numTxs
        if nTxs == 0
        then
            liftIO $ threadDelay $ 1000 * 1000
        else do
            time <- currentTick
            blk <- hoist liftIO $ Consensus.mineBlock hConsensus hEval time
            for_ blk $ \blk' -> do
                lift . P2P.broadcast $ P2P.BlockMsg (void blk')
                updateChainState

-- | Mine a block with the node’s 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadIO m
       , MonadClock m
       , Serialise tx
       , Hashable tx
       , Ord  tx
       )
    => NodeT tx s i m (Maybe (Block tx s))
mineBlock = do
    Handle{hEval, hConsensus} <- ask
    time <- currentTick
    blk <-
        hoist liftIO $ Consensus.mineBlock hConsensus hEval time
    for blk $ \blk' -> do
        updateChainState
        pure blk'

storage
    :: ( MonadIO m
       , Hashable  tx
       , Ord       tx
       )
    => Storage tx (NodeT tx s i m)
storage = Storage
    { storageApplyBlock  = applyBlock
    , storageApplyTx     = Storage.applyTx
    , storageLookupBlock = (map . map) void . Storage.lookupBlock
    , storageLookupTx    = Storage.lookupTx
    }
  where
    applyBlock blk = do
        eval <- asks hEval
        res  <- Storage.applyBlock eval blk
        res <$ when (res == Storage.Applied) updateChainState

getMempool :: MonadIO m => NodeT tx s i m (Mempool tx)
getMempool = asks hMempool >>= liftIO . atomically . Mempool.snapshot

-- | Get a state value at the given path.
getPath :: (Query s, MonadIO m) => STree.Path -> NodeT tx s i m (Maybe (QueryVal s))
getPath = queryM

getBestChain :: (Hashable tx, Ord tx, MonadIO m) => NodeT tx s i m (Blockchain tx s)
getBestChain = do
    Consensus{cScore} <- asks hConsensus
    maximumChainBy cScore

-- Internal --------------------------------------------------------------------

updateChainState :: (MonadIO m, Hashable tx, Ord tx) => NodeT tx s i m ()
updateChainState = updateM =<< chainState . Consensus.cScore =<< asks hConsensus
