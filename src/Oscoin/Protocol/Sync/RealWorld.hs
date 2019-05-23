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
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.OscoinTx
import           Oscoin.P2P as P2P
import           Oscoin.Protocol.Sync hiding (runSync)
import           Oscoin.Storage.Block.Abstract (BlockStoreReader)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Telemetry.Trace
import           Oscoin.Time (Duration, microseconds, seconds)
import           Oscoin.Time.Chrono

import           Codec.Serialise as CBOR
import           Control.Monad.Trans.Resource
import           Control.Retry
                 (RetryPolicyM, RetryStatus, exponentialBackoff, retrying)
import           Data.Conduit
import           Data.Conduit.Serialise (conduitDecodeCBOR)
import           Data.Hashable (Hashable)
import qualified Data.HashSet as Set
import           Formatting
import qualified Network.Gossip.HyParView as Gossip
import           Network.Gossip.IO.Peer (Peer(..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Conduit as C
import qualified UnliftIO.Async as Async

{------------------------------------------------------------------------------
  Scary IO implementation
------------------------------------------------------------------------------}

type instance ProtocolResponse c m (Tx c) PoW 'GetTip =
        Block c (Tx c) (Sealed c PoW)
type instance ProtocolResponse c m (Tx c) PoW 'GetBlocks =
        ConduitT () (Block c (Tx c) (Sealed c PoW)) m ()
type instance ProtocolResponse c m tx PoW 'GetBlockHeaders =
        OldestFirst [] (BlockHeader c (Sealed c PoW))


maxAllowedNetworkLatency :: Duration
maxAllowedNetworkLatency = 1 * seconds

newDataFetcher
    :: forall c tx s.
    ( Serialise tx
    , Serialise s
    , Serialise (Hash c)
    , Serialise (Beneficiary c)
    , HasHashing c
    , Serialise (ProtocolResponse c ResIO tx s 'GetTip)
    , ProtocolResponse c ResIO tx s 'GetBlockHeaders ~ OldestFirst [] (BlockHeader c (Sealed c s))
    , ProtocolResponse c ResIO tx s 'GetBlocks ~ ConduitT () (Block c tx (Sealed c s)) ResIO ()
    )
    => HTTP.Manager
    -> DataFetcher c tx s ResIO
newDataFetcher httpManager = DataFetcher $ \peer args ->
    let prettyHost = renderHost (addrHost . nodeHttpApiAddr $ peer)
        -- FIXME(adn) Support for https if needed.
        protocol   = "http://"
        prefix     = protocol <> prettyHost
    in case args of
        -- This implementation is basically 'fetchTip' function from the spec.
        SGetTip -> \() -> timed maxAllowedNetworkLatency $ do
            let url = prefix <> "/blockchain/tip"
            rq0 <- HTTP.parseUrlThrow (toS url)
            let rq = rq0 { HTTP.port = fromIntegral (addrPort . nodeHttpApiAddr $ peer) }

           -- Trying to issue a wrong response type will trigger a compilation
           -- error similar to:
           -- • Couldn't match type ‘'GetBlockHeaders’ with ‘'GetTip’
           -- Expected type: IO (ProtocolResponse r)
           -- Actual type: IO (ProtocolResponse 'GetBlockHeaders)

            liftIO $ HTTP.withResponse rq httpManager $ \res -> do
                cborBlob <- HTTP.brRead (HTTP.responseBody res)
                pure $ CBOR.deserialise . toS $ cborBlob

        -- This implementation is basically 'fetchBlocks' from the spec.
        -- N.b. It assumes oscoin#550 is implemented.
        SGetBlocks -> \Range{..} -> timed maxAllowedNetworkLatency $ do
            let url = stext
                    % "/blocks/by-height?start="
                    % int @Integer
                    % "&end="
                    % int @Integer
            rq0 <- HTTP.parseUrlThrow (formatToString url prefix start end)
            let rq = rq0 { HTTP.port = fromIntegral (addrPort . nodeHttpApiAddr $ peer) }

            response <- C.http rq httpManager
            pure (HTTP.responseBody response .| conduitDecodeCBOR)

        -- This implementation is basically 'fetchBlockHeaders' from the spec.
        -- N.b. Not only does it assumes oscoin#550 is implemented, but we are
        -- also implementing this not very efficiently, by downloading full
        -- blocks and throwing away the bodies.
        SGetBlockHeaders -> \Range{..} -> timed maxAllowedNetworkLatency $ do
            let url = stext
                    % "/blocks/by-height?start="
                    % int @Integer
                    % "&end="
                    % int @Integer
            rq0 <- HTTP.parseUrlThrow (formatToString url prefix start end)
            let rq = rq0 { HTTP.port = fromIntegral (addrPort . nodeHttpApiAddr $ peer) }

            liftIO $ HTTP.withResponse rq httpManager $ \res -> do
                cborBlob <- HTTP.brRead (HTTP.responseBody res)
                let (blks :: OldestFirst [] (Block c tx (Sealed c s))) =
                        CBOR.deserialise . toS $ cborBlob
                pure $ map blockHeader blks

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

-- | Constructs a new 'SyncContext' over @IO@.
newSyncContext
    :: ( Eq (Crypto.PublicKey c)
       , Hashable (Crypto.PublicKey c)
       , Serialise tx
       , Serialise s
       , Serialise (BlockHash c)
       , Serialise (Beneficiary c)
       , Serialise (ProtocolResponse c ResIO tx s 'GetTip)
       , ProtocolResponse c ResIO tx s 'GetBlockHeaders ~ OldestFirst [] (BlockHeader c (Sealed c s))
       , ProtocolResponse c ResIO tx s 'GetBlocks ~ ConduitT () (Block c tx (Sealed c s)) ResIO ()
       , HasHashing c
       )
    => BlockStoreReader c tx s IO
    -> [SyncEvent c tx s -> IO ()]
    -> Probe IO
    -> GossipT c ResIO (SyncContext c tx s ResIO)
newSyncContext chainReader upstreamConsumers probe = do
    env <- ask
    mgr <- liftIO (HTTP.newManager HTTP.defaultManagerSettings)
    pure $ SyncContext
        { scNu                = 5
        , scActivePeers       = Set.map peerNodeId . Gossip.active <$> P2P.getPeers' env
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
