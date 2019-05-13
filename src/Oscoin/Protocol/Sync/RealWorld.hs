{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Protocol.Sync.RealWorld where

import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Prelude

import           Oscoin.Consensus.Nakamoto (PoW)

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain.Block
                 ( Block
                 , BlockHash
                 , BlockHeader
                 , Height
                 , Sealed
                 , blockHeader
                 , blockHeight
                 )
import           Oscoin.Crypto.Hash (HasHashing)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx
import           Oscoin.P2P as P2P
import           Oscoin.Protocol.Sync
import           Oscoin.Storage.Block.Abstract (BlockStoreReader)
import           Oscoin.Telemetry.Trace
import           Oscoin.Time (Duration, microseconds, seconds)
import           Oscoin.Time.Chrono

import           Codec.Serialise as CBOR
import qualified Control.Concurrent.Async as Async
import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Network.Gossip.HyParView as Gossip
import           Network.Gossip.IO.Peer (Peer(..))
import qualified Network.HTTP.Client as HTTP

{------------------------------------------------------------------------------
  Scary IO implementation
------------------------------------------------------------------------------}

maxAllowedNetworkLatency :: Duration
maxAllowedNetworkLatency = 1 * seconds

newDataFetcherIO
    :: forall c tx s.
    ( Serialise (ProtocolResponse c tx s 'GetTip) )
    => HTTP.Manager
    -> DataFetcher c tx s IO
newDataFetcherIO httpManager = DataFetcher $ \peer -> \case
    -- This implementation is basically 'fetchTip' function from the spec.
    SGetTip -> \() -> timed maxAllowedNetworkLatency $ do
        let hn = (addrHost . nodeHttpApiAddr $ peer)
        -- FIXME(adn) Support for https if needed.
        rq0 <- HTTP.parseUrlThrow ("http://" <> toS (renderHost hn) <> "/blockchain/tip")
        let rq = rq0 { HTTP.port = fromIntegral (addrPort . nodeHttpApiAddr $ peer) }

       -- Trying to issue a wrong response type will trigger a compilation
       -- error similar to:
       -- • Couldn't match type ‘'GetBlockHeaders’ with ‘'GetTip’
       -- Expected type: IO (ProtocolResponse r)
       -- Actual type: IO (ProtocolResponse 'GetBlockHeaders)

        HTTP.withResponse rq httpManager $ \res -> do
            cborBlob <- HTTP.brRead (HTTP.responseBody res)
            pure $ CBOR.deserialise . toS $ cborBlob

    -- This implementation is basically 'fetchBlocks' from the spec.
    SGetBlocks -> \Range{..} -> notImplemented

    -- This implementation is basically 'fetchBlockHeaders' from the spec.
    SGetBlockHeaders -> \Range{..} -> notImplemented

-- | Constructs a new 'Tracer' over @IO@.
newEventTracerIO :: Probe IO -> Tracer IO
newEventTracerIO = probed

-- ^ Given a monadic IO operation and a maximum Duration for this action to
-- take, it either returns the value @a@ or a 'Timeout'.
timed :: Duration -> IO a -> IO (Either Timeout a)
timed maxTimeout action = do
    raceResult <- Async.race (threadDelay (fromIntegral $ nsToMicro maxTimeout)) action
    case raceResult of
      Left () -> pure $ Left $ MaxTimeoutExceeded maxTimeout
      Right r -> pure $ Right r
  where
      nsToMicro :: Duration -> Duration
      nsToMicro s = round (realToFrac s / (fromIntegral microseconds :: Double))

-- | Constructs a new 'SyncContext' over @IO@.
newSyncContextIO
    :: ( Eq (Crypto.PublicKey c)
       , Hashable (Crypto.PublicKey c)
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , Serialise (ProtocolResponse c tx s 'GetTip)
       , HasHashing c
       )
    => Consensus.Config
    -> BlockStoreReader c tx s IO
    -> Probe IO
    -> GossipT c IO (SyncContext c tx s IO)
newSyncContextIO config chainReader probe = do
    env <- ask
    mgr <- liftIO (HTTP.newManager HTTP.defaultManagerSettings)
    pure $ SyncContext
        { scNu          = Consensus.mutableChainDepth config
        , scActivePeers = HS.map peerNodeId . Gossip.active <$> P2P.getPeers' env
        , scDataFetcher = newDataFetcherIO mgr
        , scEventTracer = newEventTracerIO probe
        , scConcurrently = Async.forConcurrently
        -- FIXME(adn) The above is nonoptimal for this syncing code, as /any/
        -- exception will cause all the other actions to be cancelled. This is
        -- hardly what we want here, as we do want other peers to carry on
        -- undisturbed if one of the HTTP requests / CBOR deserialisation fails.
        , scLocalChainReader = chainReader
        }

