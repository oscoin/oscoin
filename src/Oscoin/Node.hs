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
    , getBestChain
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (Consensus(..))
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Consensus.Class (MonadClock(..), MonadQuery(..))
import           Oscoin.Crypto.Blockchain (Blockchain, tip)
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHeader(..), StateHash, blockHash)
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hashable, formatHash)
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
                 (MonadBlockStore(..), maximumChainBy)
import qualified Oscoin.Storage.Block.STM as BlockStore
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore

import qualified Radicle.Extended as Rad

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))

withNode
    :: (Serialise s)
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
        gen <- atomically $ BlockStore.for hBlockStore $ \bs ->
            BlockStore.getGenesisBlock bs
        Log.info (cfgLogger hConfig) ("genesis is " % formatHash) (blockHash gen)
        pure Handle{..}

    close = const $ pure ()

miner
    :: ( MonadIO            m
       , P2P.MonadBroadcast m
       , MonadClock         m
       , Serialise tx
       , Hashable  tx
       , Ord       tx
       , Serialise s
       , Ord       s
       , Hashable  st
       )
    => NodeT tx st s i m a
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
            for_ blk $ lift . P2P.broadcast . P2P.BlockMsg

-- | Mine a block with the node’s 'Consensus' on top of the best chain obtained
-- from 'MonadBlockStore' using all transactions from 'MonadMempool'.
mineBlock
    :: ( MonadIO m
       , MonadClock m
       , Serialise tx
       , Serialise s
       , Hashable tx
       , Hashable st
       , Ord  tx
       , Ord  s
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
       , Ord       tx
       , Ord       s
       , Serialise s
       )
    => Evaluator st tx o
    -> Storage tx s (NodeT tx st s i m)
storage eval = Storage
    { storageApplyBlock  = Storage.applyBlock eval
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
    :: (Serialise s, Ord s, Ord tx, Hashable tx, MonadIO m, Query st, Hashable st)
    => STree.Path -> NodeT tx st s i m (Maybe (StateHash, QueryVal st))
getPathLatest path = do
    stateHash <- blockStateHash . blockHeader . tip <$> getBestChain
    result <- getPath stateHash path
    for result $ \v ->
        pure (stateHash, v)

getBestChain :: (Hashable tx, Ord tx, Ord s, Serialise s, MonadIO m) => NodeT tx st s i m (Blockchain tx s)
getBestChain = do
    Consensus{cScore} <- asks hConsensus
    maximumChainBy cScore
