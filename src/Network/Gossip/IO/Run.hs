module Network.Gossip.IO.Run
    ( ProtocolMessage
    , Env
    , withGossip
    , broadcast
    )
where

import           Prelude

import qualified Network.Gossip.HyParView as H
import qualified Network.Gossip.HyParView.Periodic as HP
import           Network.Gossip.IO.Peer
import qualified Network.Gossip.IO.Socket as S
import           Network.Gossip.IO.Wire
import qualified Network.Gossip.Plumtree as P
import qualified Network.Gossip.Plumtree.Scheduler as PS

import           Codec.Serialise (Serialise)
import           Control.Concurrent.Async (async, uninterruptibleCancel)
import           Control.Exception.Safe (bracket, onException, tryAny)
import           Data.Bifunctor (second)
import           Data.ByteString (ByteString)
import           Data.Foldable (for_, toList)
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)
import           Network.Socket (HostName, PortNumber)
import qualified System.Random.SplitMix as SplitMix

data ProtocolMessage n =
      ProtocolPlumtree  (P.Message n)
    | ProtocolHyParView (H.RPC     n)
    deriving (Eq, Generic)

instance (Eq n, Hashable n, Serialise n) => Serialise (ProtocolMessage n)

data Env n = Env
    { envPlumtree      :: P.Handle  (Peer n)
    , envHyParView     :: H.Handle  (Peer n)
    , envScheduler     :: PS.Handle (Peer n)
    , envIO            :: S.Handle  n        (ProtocolMessage (Peer n))
    , envApplyMessage  :: P.MessageId -> ByteString -> IO P.ApplyResult
    , envLookupMessage :: P.MessageId -> IO (Maybe ByteString)
    }

withGossip
    :: (Eq n, Hashable n, Traversable t)
    => Peer n
    -- ^ Self
    -> H.Config
    -- ^ "Network.Gossip.HyParView" settings
    -> HP.Config
    -- ^ "Network.Gossip.HyParView.Periodic" settings
    -> PS.LazyFlushInterval
    -- ^ Flush interval for "Network.Gossip.ProtocolMessage.Scheduler"
    -> S.Handshake n (ProtocolMessage (Peer n))
    -- ^ Handshake
    -> (P.MessageId -> ByteString -> IO P.ApplyResult)
    -- ^ Apply message
    -> (P.MessageId -> IO (Maybe ByteString))
    -- ^ Lookup message
    -> HostName
    -- ^ 'HostName' to bind to
    -> PortNumber
    -- ^ 'PortNumber' to listen on
    -> t (Peer n)
    -- ^ Intial contacts
    -> (Env n -> IO a)
    -> IO a
withGossip self
           hcfg
           hpcfg
           flushInterval
           handshake
           envApplyMessage
           envLookupMessage
           host
           port
           contacts
           k
    = do
    envPlumtree  <- P.new self
    envHyParView <- H.new self hcfg =<< SplitMix.initSMGen
    envIO        <- S.new handshake
    envScheduler <- PS.new flushInterval
    let env = Env {..}

    PS.withScheduler envScheduler (sendIHaves env) $
        HP.withPeriodic hpcfg (runHyParView env)   . const $
        bracket (listen env) uninterruptibleCancel . const $ do
            bootstrap env
            k env
  where
    sendIHaves env to xs =
        runNetwork env $
            for_ xs $ \ihave ->
                S.send to $ WirePayload (ProtocolPlumtree (P.IHaveM ihave))

    listen env = async $
        runNetwork env (S.listen (evalNetwork env) host port)

    bootstrap env = do
        peers <-
            runHyParView env $ do
                H.joinAny (toList contacts)
                H.getPeers
        runPlumtree env $ P.resetPeers peers

broadcast :: (Eq n, Hashable n) => Env n -> P.MessageId -> ByteString -> IO ()
broadcast env mid msg = runPlumtree env $ P.broadcast mid msg

evalPlumtree
    :: (Eq n, Hashable n)
    => Env n
    -> P.PlumtreeC (Peer n) a
    -> IO a
evalPlumtree env@Env { envApplyMessage, envLookupMessage } = go
  where
    go = \case
        P.ApplyMessage mid v k ->
            envApplyMessage mid v >>= k >>= go

        P.LookupMessage mid k ->
            envLookupMessage mid >>= k >>= go

        P.SendEager to msg k -> do
            runNetwork env (S.send to (mkWire msg))
                `onException` runHyParView env (H.eject to)
            k >>= go

        P.SendLazy to msg k -> do
            runScheduler env $ PS.sendLazy to msg
            k >>= go

        P.Later t mid action k -> do
            runScheduler env $ PS.later t mid (runPlumtree env action)
            k >>= go

        P.Cancel mid k -> do
            runScheduler env $ PS.cancel mid
            k >>= go

        P.Done a -> pure a

    mkWire = WirePayload . ProtocolPlumtree

evalHyParView
    :: (Eq n, Hashable n)
    => Env n
    -> H.HyParViewC (Peer n) a
    -> IO a
evalHyParView env = go
  where
    go = \case
        H.ConnectionOpen to k -> do
            conn <-
                    second (const $ mkConn to)
                <$> tryAny (runNetwork env (S.connect (evalNetwork env) to))
            k conn >>= go

        H.SendAdHoc rpc k -> do
            -- swallow exceptions?
            runNetwork env $ S.send (H.rpcRecipient rpc) (mkWire rpc)
            k >>= go

        H.NeighborUp n k -> do
            runPlumtree env $ P.neighborUp n
            k >>= go

        H.NeighborDown n k -> do
            runPlumtree env $ P.neighborDown n
            k >>= go

        H.Done a -> pure a

    mkConn to = H.Connection
        { connSend  = \rpc ->
            runNetwork env $ S.send (H.rpcRecipient rpc) (mkWire rpc)
        , connClose = runNetwork env $ S.disconnect to
        }

    mkWire = WirePayload . ProtocolHyParView

evalNetwork
    :: (Eq n, Hashable n)
    => Env n
    -> S.NetworkC n (ProtocolMessage (Peer n)) a
    -> IO a
evalNetwork env = go
  where
    go = \case
        S.PayloadReceived _from (ProtocolHyParView p) k -> do
            runHyParView env $ H.receive p
            k >>= go

        S.PayloadReceived _from (ProtocolPlumtree p) k -> do
            runPlumtree  env $ P.receive p
            k >>= go

        S.ConnectionLost to k -> do
            runHyParView env $ H.eject to
            k >>= go

        S.Done a -> pure a

--------------------------------------------------------------------------------

runHyParView :: (Eq n, Hashable n) => Env n -> H.HyParView (Peer n) a -> IO a
runHyParView env@Env { envHyParView } ma =
    H.runHyParView envHyParView ma >>= evalHyParView env

runPlumtree :: (Eq n, Hashable n) => Env n -> P.Plumtree (Peer n) a -> IO a
runPlumtree env@Env { envPlumtree } ma =
    P.runPlumtree envPlumtree ma >>= evalPlumtree env

runNetwork
    :: (Eq n, Hashable n)
    => Env n
    -> S.Network n (ProtocolMessage (Peer n)) a
    -> IO a
runNetwork env@Env { envIO } ma =
    S.runNetwork envIO ma >>= evalNetwork env

runScheduler :: Env n -> PS.SchedulerT (Peer n) IO a -> IO a
runScheduler Env { envScheduler } ma =
    PS.runSchedulerT envScheduler ma
