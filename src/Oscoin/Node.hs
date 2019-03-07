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

import           Oscoin.Clock (MonadClock(..))
import           Oscoin.Consensus (Consensus(..), ValidationError)
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Crypto.Blockchain (TxLookup)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block(..)
                 , BlockHash
                 , BlockHeader(..)
                 , Depth
                 , Sealed
                 , StateHash
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
import qualified Oscoin.P2P as P2P
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import           Oscoin.Storage.State.Class (MonadStateStore(..))
import           Oscoin.Telemetry (NotableEvent(..))
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (ftag, stext, (%))
import qualified Oscoin.Telemetry.Logging as Log
import qualified Oscoin.Time.Chrono as Chrono

import qualified Oscoin.Protocol as Protocol
import           Oscoin.Storage.Block.Abstract
                 (BlockStoreReader, hoistBlockStoreReader)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Storage.State as StateStore
import qualified Oscoin.Storage.State.Class as StateStoreClass

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Retry (constantDelay, limitRetries, recovering)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Lens.Micro ((^.))

withNode
    :: Log.Buildable (Hash c)
    => Config
    -> i
    -> Mempool.Handle c tx
    -> StateStore.Handle c st
    -> BlockStoreReader c tx s IO
    -> Protocol.Handle c tx s IO
    -> Evaluator st tx RadicleTx.Output
    -> Consensus c tx s (NodeT c tx st s i IO)
    -> (Handle c tx st s i -> IO a)
    -> IO a
withNode hConfig hNodeId hMempool hStateStore hBlockStore hProtocol hEval hConsensus =
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
       , MonadMask            m
       , P2P.MonadBroadcast c m
       , MonadClock           m
       , Serialise   tx
       , Hashable  c tx
       , Serialise s
       , Hashable  c st
       , Ord (Hash c)
       , Serialise (Hash c)
       , AuthTree.MerkleHash (Hash c)
       , Log.Buildable (Hash c)
       )
    => NodeT c tx st s i m a
miner = do
    Handle{hConfig} <- ask
    let
        tele :: HasCallStack => NotableEvent -> IO ()
        tele = Telemetry.emit (cfgTelemetry hConfig)
    forever $ do
        nTxs <- Mempool.numTxs
        if nTxs == 0 && cfgNoEmptyBlocks hConfig
        then
            liftIO $ threadDelay $ 1000 * 1000
        else do
            blk <- mineBlock
            for_ blk $ \b -> do
                let bhash = blockHash b
                liftIO $ tele (BlockMinedEvent bhash)
                lift (tryBroadcast b) >>= liftIO . tele . \case
                    Left  e  -> BlockBroadcastFailedEvent bhash (toException e)
                    Right () -> BlockBroadcastEvent bhash
  where
    tryBroadcast
        :: ( MonadIO              m
           , MonadMask            m
           , P2P.MonadBroadcast c m
           , Serialise s
           , Serialise   tx
           , Hashable  c tx
           )
        => Block c tx (Sealed c s)
        -> m (Either IOException ())
    tryBroadcast blk =
        let
            policy = limitRetries 5 <> constantDelay 10000
            -- TODO(kim): gossip should allow us to intercept the `NoSuchPeer`
            -- error here, too
            hdlrs  = [const . Handler $ \(_ :: IOException) -> pure True]
         in
            try . recovering policy hdlrs . const $
                P2P.broadcast (P2P.BlockMsg blk)

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
       , AuthTree.MerkleHash (Hash c)
       )
    => NodeT c tx st s i m (Maybe (Block c tx (Sealed c s)))
mineBlock = do
    Handle{hEval, hConsensus, hBlockStore, hProtocol} <- ask
    time <- currentTick
    res  <- hoist liftIO $ Consensus.mineBlock (hoistBlockStoreReader lift hBlockStore)
                                               hConsensus
                                               hEval
                                               time
    case res of
      Nothing -> pure Nothing
      Just (blk, st', receipts) -> do
          -- NOTE(adn) Here we should dispatch the block and wait for the
          -- result: if the block hasn't been inserted (for example due to
          -- a validation error) we shouldn't proceed with all these other
          -- side effects. Ideally 'dispatchBlockSync' should return some kind
          -- of 'Either Error ()'.
          liftIO $ Protocol.dispatchBlockSync hProtocol blk
          Mempool.delTxs (blockData blk)
          StateStoreClass.storeState st'
          for_ receipts ReceiptStore.addReceipt
          pure (Just blk)

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
    => (Block c tx (Sealed c s) -> Either (ValidationError c) ())
    -> Storage c tx s (NodeT c tx st s i m)
storage validateBasic = Storage
    { storageApplyBlock = \blk -> do
        Handle{hProtocol, hBlockStore, hEval, hConfig} <- ask
        let dispatchBlock = liftIO . Protocol.dispatchBlockAsync hProtocol
        let consensusConfig =  cfgConsensusConfig hConfig
        Storage.applyBlock (hoistBlockStoreReader liftIO hBlockStore) dispatchBlock hEval validateBasic consensusConfig blk
    , storageApplyTx     = \tx -> do
        bs <- asks hBlockStore
        Storage.applyTx (hoistBlockStoreReader liftIO bs) tx
    , storageLookupBlock = \blk -> do
        bs <- asks hBlockStore
        BlockStore.lookupBlock (hoistBlockStoreReader liftIO bs) blk
    , storageLookupTx    = \tx -> do
        bs <- asks hBlockStore
        Storage.lookupTx (hoistBlockStoreReader liftIO bs) tx
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
    -> [Text]
    -> NodeT c tx st s i m (Maybe (QueryVal st))
getPath sh p = do
    result <- lookupState sh
    pure $ query p =<< result

-- | Get a value from the latest state.
getPathLatest
    :: ( MonadIO m
       , Query st
       , Hashable c st
       , Ord (StateHash c)
       )
    => [Text]
    -> NodeT c tx st s i m (Maybe (StateHash c, QueryVal st))
getPathLatest path = do
    bs <- asks hBlockStore
    stateHash <- blockStateHash . blockHeader <$> BlockStore.getTip (hoistBlockStoreReader liftIO bs)
    result <- getPath stateHash path
    for result $ \v ->
        pure (stateHash, v)

getBlocks
    :: MonadIO m
    => Depth
    -> NodeT c tx st s i m [Block c tx (Sealed c s)]
getBlocks d =
  Chrono.toNewestFirst <$> withBlockStore (`BlockStore.getBlocksByDepth` d)

lookupTx :: (MonadIO m) => Hashed c tx -> NodeT c tx st s i m (Maybe (TxLookup c tx))
lookupTx tx = withBlockStore (`BlockStore.lookupTx` tx)

lookupBlock
    :: (MonadIO m)
    => BlockHash c
    -> NodeT c tx st s i m (Maybe (Block c tx (Sealed c s)))
lookupBlock h = withBlockStore (`BlockStore.lookupBlock` h)
