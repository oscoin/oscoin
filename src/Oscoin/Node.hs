module Oscoin.Node
    ( Config (..)
    , GlobalConfig (..)
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
    , getTip
    , lookupTx
    , lookupReceipt
    , lookupBlock
    , lookupBlockByHeight
    , lookupBlocksByHeight
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (MonadClock(..))
import           Oscoin.Configuration (renderEnvironment, renderNetwork)
import           Oscoin.Consensus (Consensus(..), ValidationError)
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Crypto.Blockchain (TxLookup)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Blockchain.Eval (Receipt)
import           Oscoin.Crypto.Hash (Hash, Hashable, Hashed, formatHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.Query
import           Oscoin.Data.Tx
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Trans
import qualified Oscoin.P2P as P2P
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import qualified Oscoin.Storage.Ledger as Ledger
import           Oscoin.Telemetry (NotableEvent(..))
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (ftag, stext, (%))
import qualified Oscoin.Telemetry.Logging as Log
import qualified Oscoin.Time.Chrono as Chrono

import qualified Oscoin.Protocol as Protocol
import qualified Oscoin.Storage.Block.Abstract as BlockStore

import           Codec.Serialise
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Retry (constantDelay, limitRetries, recovering)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Lens.Micro ((^.))

withNode
    :: (Log.Buildable (Hash c))
    => Config c
    -> i
    -> Mempool.Handle c tx
    -> Ledger.Ledger c s tx (TxOutput c tx) (TxState c tx) IO
    -> Protocol.Handle c tx s IO
    -> Consensus c tx s (NodeT c tx s i IO)
    -> (Handle c tx s i -> IO a)
    -> IO a
withNode hConfig hNodeId hMempool hLedger hProtocol hConsensus =
    bracket open close
  where
    open = do
        gen <- liftIO (BlockStore.getGenesisBlock $ Ledger.blockStoreReader hLedger)
        let
            GlobalConfig { globalEnv             = env
                         , globalLogicalNetwork  = lnet
                         , globalPhysicalNetwork = pnet
                         } = cfgGlobalConfig hConfig
         in
            Log.info (cfgTelemetry hConfig ^. Log.loggerL)
                     "node starting"
                     ( ftag "env" % stext
                     % " "
                     % ftag "logical_network" % stext
                     % " "
                     % ftag "physical_network" % stext
                     % " "
                     % ftag "genesis" % formatHash
                     )
                     (renderEnvironment env)
                     (renderNetwork     lnet)
                     (P2P.renderNetwork pnet)
                     (blockHash gen)
        pure Handle{..}

    close = const $ pure ()

miner
    :: ( MonadIO              m
       , MonadMask            m
       , P2P.MonadBroadcast c m
       , MonadClock           m
       , Serialise   tx
       , Serialise (Beneficiary c)
       , Hashable  c tx
       , Serialise s
       , Hashable  c (TxState c tx)
       , Serialise (Hash c)
       , AuthTree.MerkleHash (Hash c)
       , Log.Buildable (Hash c)
       )
    => NodeT c tx s i m a
miner = do
    metrics <- asks (cfgTelemetry . hConfig)
    forever $ do
        blk <- mineBlock
        for_ blk $ \b -> do
            let bhash = blockHash b
            liftIO $ Telemetry.emit metrics (BlockMinedEvent bhash)
            lift (tryBroadcast b) >>= liftIO . Telemetry.emit metrics . \case
                Left  e  -> BlockBroadcastFailedEvent bhash (toException e)
                Right () -> BlockBroadcastEvent bhash
  where
    tryBroadcast
        :: ( MonadIO              m
           , MonadMask            m
           , P2P.MonadBroadcast c m
           , Serialise (Beneficiary c)
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
       , Serialise (Beneficiary c)
       , Hashable c tx
       , Hashable c (TxState c tx)
       , Serialise (Hash c)
       , AuthTree.MerkleHash (Hash c)
       )
    => NodeT c tx s i m (Maybe (Block c tx (Sealed c s)))
mineBlock = do
    Handle{hConsensus, hProtocol, hConfig} <- ask
    time <- currentTick
    ledger <- getLedger
    maybeBlock <- hoist liftIO $ Consensus.mineBlock
        ledger
        hConsensus
        time
        (cfgBeneficiary hConfig)

    -- NOTE(adn) Here we should dispatch the block and wait for the
    -- result: if the block hasn't been inserted (for example due to
    -- a validation error) we shouldn't proceed with all these other
    -- side effects. Ideally 'dispatchBlockSync' should return some kind
    -- of 'Either Error ()'.
    forM maybeBlock $ \blk -> do
        liftIO $ Protocol.dispatchBlockSync hProtocol blk
        pure blk

storage
    :: ( MonadIO m
       , Hashable  c tx
       , Serialise tx
       , Serialise s
       , Serialise (Hash c)
       , Serialise (BlockData c tx)
       , Log.Buildable (Hash c)
       )
    => (Block c tx (Sealed c s) -> Either (ValidationError c) ())
    -> Storage c tx s (NodeT c tx s i m)
storage validateBasic = Storage
    { storageApplyBlock = \blk -> do
        Handle{hProtocol, hConfig} <- ask
        let consensusConfig =  cfgConsensusConfig hConfig
        let protoHandle = Protocol.hoistHandle liftIO hProtocol
        Storage.applyBlock protoHandle validateBasic consensusConfig blk
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

getMempool :: MonadIO m => NodeT c tx s i m (Mempool c tx)
getMempool = asks hMempool >>= liftIO . atomically . Mempool.snapshot

-- | Get a value from the given state hash.
getPath
    :: ( Query st
       , st ~ TxState c tx
       , MonadIO m
       )
    => StateHash c
    -> [Text]
    -> NodeT c tx s i m (Maybe (QueryVal st))
getPath stateHash p = do
    ledger <- getLedger
    result <- Ledger.lookupState ledger (Crypto.toHashed stateHash)
    pure $ query p =<< result

-- | Get a value from the latest state.
getPathLatest
    :: ( MonadIO m
       , Query st
       , st ~ TxState c tx
       , Crypto.Hashable c tx
       )
    => [Text]
    -> NodeT c tx s i m (Maybe (QueryVal st))
getPathLatest path = do
    ledger <- getLedger
    Ledger.getTipWithState ledger >>= \case
        Left _err -> pure Nothing
        Right (_, st) -> pure $ query path st

getBlocks
    :: MonadIO m
    => Depth
    -> NodeT c tx s i m [Block c tx (Sealed c s)]
getBlocks d = do
    bs <- getBlockStoreReader
    Chrono.toNewestFirst <$> BlockStore.getBlocksByDepth bs d

getTip
    :: MonadIO m
    => NodeT c tx s i m (Block c tx (Sealed c s))
getTip = getBlockStoreReader >>= BlockStore.getTip

lookupTx :: (MonadIO m) => Hashed c tx -> NodeT c tx s i m (Maybe (TxLookup c tx))
lookupTx tx = do
    bs <- getBlockStoreReader
    BlockStore.lookupTx bs tx

lookupReceipt
    :: (MonadIO m, Crypto.Hashable c tx)
    => Hashed c tx
    -> NodeT c tx s i m (Maybe (Receipt c tx (TxOutput c tx)))
lookupReceipt txHash = do
    ledger <- getLedger
    Ledger.lookupReceipt ledger txHash

lookupBlock
    :: (MonadIO m)
    => BlockHash c
    -> NodeT c tx s i m (Maybe (Block c tx (Sealed c s)))
lookupBlock h = do
    bs <- getBlockStoreReader
    BlockStore.lookupBlock bs h

lookupBlockByHeight
    :: (MonadIO m)
    => Height
    -> NodeT c tx s i m (Maybe (Block c tx (Sealed c s)))
lookupBlockByHeight h = do
    bs <- getBlockStoreReader
    BlockStore.lookupBlockByHeight bs h

lookupBlocksByHeight
    :: (MonadIO m)
    => (Height, Height)
    -> NodeT c tx s i m (Chrono.OldestFirst [] (Block c tx (Sealed c s)))
lookupBlocksByHeight range = do
    bs <- getBlockStoreReader
    BlockStore.lookupBlocksByHeight bs range
