{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Protocol.Sync.RealWorld (
      syncNode
    , newSyncContext
    , runSync
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Nakamoto (PoW)
import           Oscoin.Crypto.Blockchain.Block
                 ( Beneficiary
                 , Block
                 , BlockHash
                 , BlockHeader
                 , Sealed
                 , blockHeader
                 )
import           Oscoin.Crypto.Hash (HasHashing, Hash)
import           Oscoin.P2P (Addr(..), nodeHttpApiAddr, renderHost)
import           Oscoin.Protocol.Sync hiding (runSync)
import           Oscoin.Storage.Block.Abstract (BlockStoreReader)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Telemetry.Trace
import           Oscoin.Time (Duration, microseconds, seconds)

import           Codec.Serialise as CBOR
import           Control.Monad.Trans.Resource hiding (throwM)
import           Control.Retry
                 (RetryPolicyM, RetryStatus, exponentialBackoff, retrying)
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.Serialise (conduitDecodeCBOR)
import           Formatting
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Conduit as C
import qualified Oscoin.API.Types as API
import qualified UnliftIO.Async as Async

{------------------------------------------------------------------------------
  Scary IO implementation
------------------------------------------------------------------------------}

type instance ProtocolResponse c m tx PoW 'GetTip =
        Block c tx (Sealed c PoW)
type instance ProtocolResponse c m tx PoW 'GetBlocks =
        ConduitT () (Block c tx (Sealed c PoW)) m ()
type instance ProtocolResponse c m tx PoW 'GetBlockHeaders =
        [BlockHeader c (Sealed c PoW)]

-- | The maximum delay (in terms of response time) we can tolerate from peers.
maxAllowedNetworkLatency :: Duration
maxAllowedNetworkLatency = 30 * seconds

newDataFetcher
    :: forall c tx s.
    ( Serialise tx
    , Serialise s
    , Serialise (Hash c)
    , Serialise (Beneficiary c)
    , HasHashing c
    , Serialise (ProtocolResponse c ResIO tx s 'GetTip)
    , ProtocolResponse c ResIO tx s 'GetBlockHeaders ~ [BlockHeader c (Sealed c s)]
    , ProtocolResponse c ResIO tx s 'GetBlocks ~ ConduitT () (Block c tx (Sealed c s)) ResIO ()
    )
    => HTTP.Manager
    -> DataFetcher c tx s ResIO
newDataFetcher httpManager = DataFetcher $ \peer args ->
    let peerAddr   = nodeHttpApiAddr peer
        prettyHost = renderHost (addrHost peerAddr)
        -- FIXME(adn) Support for https if needed.
        protocol   = "http://"
        prefix     = protocol <> prettyHost
    in case args of
        -- This implementation is basically 'fetchTip' function from the spec.
        SGetTip -> \() -> do
            r <- timed' GetTip peerAddr $ catchNetworkError GetTip peerAddr $ do
                let url = prefix <> "/blockchain/tip"
                rq0 <- HTTP.parseUrlThrow (toS url)
                let rq = rq0 { HTTP.port = fromIntegral (addrPort peerAddr) }

                -- Trying to issue a wrong response type will trigger a compilation
                -- error similar to:
                -- • Couldn't match type ‘'GetBlockHeaders’ with ‘'GetTip’
                -- Expected type: IO (ProtocolResponse r)
                -- Actual type: IO (ProtocolResponse 'GetBlockHeaders)

                liftIO $ HTTP.withResponse rq httpManager $ \res -> do
                    cborBlob <- HTTP.brRead (HTTP.responseBody res)
                    case CBOR.deserialise . toS $ cborBlob of
                      API.Ok t    -> pure t
                      API.Err err -> throwM $ RequestNetworkError GetTip peerAddr (toS err)
            pure $ join r

        -- This implementation is basically 'fetchBlocks' from the spec.
        SGetBlocks -> \Range{..} -> do
            r <- timed' GetBlocks peerAddr $ catchNetworkError GetBlocks peerAddr $ do
                let url = stext
                        % "/blocks/by-height?start="
                        % int @Integer
                        % "&end="
                        % int @Integer
                rq0 <- HTTP.parseUrlThrow (formatToString url prefix start end)
                let rq = rq0 { HTTP.port = fromIntegral (addrPort . nodeHttpApiAddr $ peer) }

                response <- C.http rq httpManager
                pure (  HTTP.responseBody response
                     .| mapOutput (\r -> case r of
                                             API.Ok o  -> o :: [Block c tx (Sealed c s)]
                                             API.Err e -> panic e
                                  ) conduitDecodeCBOR
                     .| C.concat)
            pure $ join r

        -- This implementation is basically 'fetchBlockHeaders' from the spec.
        -- N.b. We are implementing this very inefficiently by downloading full
        -- blocks and throwing away the bodies, because we don't have first-class
        -- support to retrieve only the block headers from the HTTP API.
        SGetBlockHeaders -> \Range{..} -> do
            r <- timed' GetBlockHeaders peerAddr $ catchNetworkError GetBlockHeaders peerAddr $ do
                let url = stext
                        % "/blocks/by-height?start="
                        % int @Integer
                        % "&end="
                        % int @Integer
                rq0 <- HTTP.parseUrlThrow (formatToString url prefix start end)
                let rq = rq0 { HTTP.port = fromIntegral (addrPort . nodeHttpApiAddr $ peer) }

                liftIO $ HTTP.withResponse rq httpManager $ \res -> do
                    cborBlob <- HTTP.brRead (HTTP.responseBody res)
                    let (blks :: API.Result [Block c tx (Sealed c s)]) =
                            CBOR.deserialise . toS $ cborBlob
                    case blks of
                      API.Ok b -> pure $ map blockHeader b
                      API.Err err -> throwM $ RequestNetworkError GetTip peerAddr (toS err)
            pure $ join r

-- | Catches any exception thrown from the inner action and convert that into
-- a 'SyncError' by wrapping the exception into a 'RequestNetworkError'
-- constructor.
catchNetworkError
    :: MonadCatch m
    => ProtocolRequest
    -> Addr
    -> m a
    -> m (Either SyncError a)
catchNetworkError protoReq addr action = do
    res <- try action
    case res of
      Left (ex :: SomeException) ->
          pure $ Left $ RequestNetworkError protoReq addr (displayException ex)
      Right r -> pure $ Right r

-- | Constructs a new 'Tracer' over @IO@.
newEventTracer :: Probe IO -> Tracer IO
newEventTracer = probed

-- ^ Given a monadic IO operation and a maximum Duration for this action to
-- take, it either returns the value @a@ or a 'Timeout'.
timed :: MonadUnliftIO m => Duration -> m a -> m (Either Timeout a)
timed maxTimeout action = do
    raceResult <- Async.race (liftIO (threadDelay (fromIntegral $ nsToMicro maxTimeout))) action
    case raceResult of
      Left () -> pure $ Left $ MaxTimeoutExceeded maxTimeout
      Right r -> pure $ Right r
  where
      nsToMicro :: Duration -> Duration
      nsToMicro s = round (realToFrac s / (fromIntegral microseconds :: Double))

timed'
    :: MonadUnliftIO m
    => ProtocolRequest
    -> Addr
    -> m a
    -> m (Either SyncError a)
timed' req addr action = do
    res <- timed maxAllowedNetworkLatency action
    case res of
        Left err -> pure $ Left $ RequestTimeout req addr err
        Right r  -> pure $ Right r

-- | Constructs a new 'SyncContext' over @IO@.
newSyncContext
    :: ( Serialise tx
       , Serialise s
       , Serialise (BlockHash c)
       , Serialise (Beneficiary c)
       , Serialise (ProtocolResponse c ResIO tx s 'GetTip)
       , ProtocolResponse c ResIO tx s 'GetBlockHeaders ~ [BlockHeader c (Sealed c s)]
       , ProtocolResponse c ResIO tx s 'GetBlocks ~ ConduitT () (Block c tx (Sealed c s)) ResIO ()
       , HasHashing c
       )
    => IO (ActivePeers c)
    -- ^ An action to fetch the currently active peers for this node.
    -- In a realWorld setting this would probably be something
    -- like: Set.map peerNodeId . Gossip.active <$> P2P.getPeers' env
    -> BlockStoreReader c tx s IO
    -> [SyncEvent c tx s -> IO ()]
    -> Probe IO
    -> IO (SyncContext c tx s ResIO)
newSyncContext getActive chainReader upstreamConsumers probe = do
    mgr <- liftIO (HTTP.newManager HTTP.defaultManagerSettings)
    pure $ SyncContext
        { scNu                = 5
        , scActivePeers       = liftIO getActive
        , scDataFetcher       = newDataFetcher mgr
        , scEventTracer       = liftIO . newEventTracer probe
        , scConcurrently      = Async.forConcurrently
        -- FIXME(adn) The above is nonoptimal for this syncing code, as /any/
        -- exception will cause all the other actions to be cancelled. This is
        -- hardly what we want here, as we do want other peers to carry on
        -- undisturbed if one of the HTTP requests / CBOR deserialisation fails.
        , scLocalChainReader  = BlockStore.hoistBlockStoreReader liftIO chainReader
        , scUpstreamConsumers = map (\f -> liftIO . f) upstreamConsumers
        }

{------------------------------------------------------------------------------
  Syncing a node in IO
------------------------------------------------------------------------------}

-- | Runs the 'Sync' action and returns its result. Any error arising is
-- rethrown as an 'Exception'.
runSync
    :: SyncContext c tx s ResIO
    -> Sync c tx s ResIO a
    -> IO a
runSync syncContext (Sync s) = do
    lower <- runResourceT $ runReaderT (runExceptT s) syncContext
    case lower of
      Left err -> throwIO err
      Right r  -> pure r

-- | Syncs a node, in a monad which can do @IO@.
syncNode
    :: forall c tx s m.
       ( ProtocolResponse c m tx s 'GetTip ~ Block c tx (Sealed c s)
       , ProtocolResponse c m tx s 'GetBlocks ~ ConduitT () (Block c tx (Sealed c s)) m ()
       , MonadIO m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (Beneficiary c)
       )
    => Sync c tx s m ()
syncNode = do
    ctx <- ask
    retrying policy (checkSynced ctx) $ \ _retryStatus -> do
        remoteTip <- getRemoteTip
        localTip  <- lift (BlockStore.getTip (scLocalChainReader ctx))
        for_ (range localTip remoteTip) syncBlocks
  where
      policy :: RetryPolicyM (Sync c tx s m)
      policy = exponentialBackoff 100_000

      -- Retry syncing if 'isDone' returns False.
      checkSynced
          :: SyncContext c tx s m
          -> RetryStatus
          -> ()
          -> Sync c tx s m Bool
      checkSynced ctx _retryStatus () = do
          -- We need to refetch the tips.
          remoteTip <- getRemoteTip
          localTip  <- lift (BlockStore.getTip (scLocalChainReader ctx))
          pure $ not (isDone (scNu ctx) localTip remoteTip)
