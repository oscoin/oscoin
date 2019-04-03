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
    , lookupReceipt
    , lookupBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (MonadClock(..))
import           Oscoin.Configuration (renderEnvironment)
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
import           Oscoin.Crypto.Blockchain.Eval (Evaluator, Receipt)
import           Oscoin.Crypto.Hash
                 (HasHashing, Hash, Hashable, Hashed, formatHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.Query
import qualified Oscoin.Data.RadicleTx as RadicleTx
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Node.Trans
import qualified Oscoin.P2P as P2P
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import           Oscoin.Storage.HashStore
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

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Retry (constantDelay, limitRetries, recovering)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Lens.Micro ((^.))

withNode
    :: (Log.Buildable (Hash c), HasHashing c)
    => Config
    -> i
    -> Mempool.Handle c tx
    -> HashStore c st IO
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
        hReceiptStore <- ReceiptStore.newReceiptStoreIO
        gen <- liftIO (BlockStore.getGenesisBlock hBlockStore)
        Log.info (cfgTelemetry hConfig ^. Log.loggerL)
                 "running in"
                 (ftag "env" % stext) (renderEnvironment $ cfgEnv hConfig)
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
       , Serialise (Hash c)
       , AuthTree.MerkleHash (Hash c)
       )
    => NodeT c tx st s i m (Maybe (Block c tx (Sealed c s)))
mineBlock = do
    Handle{hEval, hConsensus, hProtocol} <- ask
    stateStore <- getStateStore
    time <- currentTick
    bs <- getBlockStoreReader
    maybeBlock <- hoist liftIO $ Consensus.mineBlock
        bs
        stateStore
        hConsensus
        hEval
        time
    -- NOTE(adn) Here we should dispatch the block and wait for the
    -- result: if the block hasn't been inserted (for example due to
    -- a validation error) we shouldn't proceed with all these other
    -- side effects. Ideally 'dispatchBlockSync' should return some kind
    -- of 'Either Error ()'.
    forM maybeBlock $ \(blk, st, receipts) -> do
        stateStore' <- getStateStore
        storeHashContent stateStore' st
        receiptStore <- getReceiptStore
        for_ receipts $ ReceiptStore.storeReceipt receiptStore
        liftIO $ Protocol.dispatchBlockSync hProtocol blk
        pure blk

storage
    :: ( MonadIO m
       , Hashable  c tx
       , Serialise tx
       , Serialise s
       , Serialise (Hash c)
       , Log.Buildable (Hash c)
       )
    => (Block c tx (Sealed c s) -> Either (ValidationError c) ())
    -> Storage c tx s (NodeT c tx st s i m)
storage validateBasic = Storage
    { storageApplyBlock = \blk -> do
        Handle{hProtocol, hConfig} <- ask
        let dispatchBlock = liftIO . Protocol.dispatchBlockAsync hProtocol
        let consensusConfig =  cfgConsensusConfig hConfig
        bs <- getBlockStoreReader
        Storage.applyBlock bs dispatchBlock validateBasic consensusConfig blk
    , storageApplyTx     = \tx -> do
        bs <- getBlockStoreReader
        Storage.applyTx bs tx
    , storageLookupBlock = \blk -> do
        bs <- getBlockStoreReader
        BlockStore.lookupBlock bs blk
    , storageLookupTx    = \tx -> do
        bs <- getBlockStoreReader
        Storage.lookupTx bs tx
    }

getMempool :: MonadIO m => NodeT c tx st s i m (Mempool c tx)
getMempool = asks hMempool >>= liftIO . atomically . Mempool.snapshot

-- | Get a value from the given state hash.
getPath
    :: ( Query st
       , MonadIO m
       )
    => StateHash c
    -> [Text]
    -> NodeT c tx st s i m (Maybe (QueryVal st))
getPath stateHash p = do
    stateStore <- getStateStore
    result <- lookupHashContent stateStore (Crypto.toHashed stateHash)
    pure $ query p =<< result

-- | Get a value from the latest state.
getPathLatest
    :: ( MonadIO m
       , Query st
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
getBlocks d = do
    bs <- getBlockStoreReader
    Chrono.toNewestFirst <$> BlockStore.getBlocksByDepth bs d

lookupTx :: (MonadIO m) => Hashed c tx -> NodeT c tx st s i m (Maybe (TxLookup c tx))
lookupTx tx = do
    bs <- getBlockStoreReader
    BlockStore.lookupTx bs tx

lookupReceipt :: (MonadIO m) => Hashed c tx -> NodeT c tx st s i m (Maybe (Receipt c tx RadicleTx.Output))
lookupReceipt txHash = do
    receiptStore <- getReceiptStore
    ReceiptStore.lookupReceipt receiptStore txHash

lookupBlock
    :: (MonadIO m)
    => BlockHash c
    -> NodeT c tx st s i m (Maybe (Block c tx (Sealed c s)))
lookupBlock h = do
    bs <- getBlockStoreReader
    BlockStore.lookupBlock bs h
