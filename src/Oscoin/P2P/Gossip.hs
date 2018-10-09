module Oscoin.P2P.Gossip
    ( GossipT
    , Gossip
    , Handle
    , Wire

    , Peer
    , knownPeer

    , runGossipT
    , withGossip

    , listen
    , broadcast

    -- * Re-exports
    , WireMessage
    , ProtocolMessage
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (MonadClock)
import           Oscoin.Logging (Logger)

import qualified Oscoin.P2P.Gossip.Broadcast as Plum
import           Oscoin.P2P.Gossip.IO (Peer, knownPeer)
import qualified Oscoin.P2P.Gossip.IO as IO
import qualified Oscoin.P2P.Gossip.Membership as Hypa
import           Oscoin.P2P.Gossip.Wire
import           Oscoin.P2P.Handshake (Handshake)
import           Oscoin.P2P.Types (NodeId)

import           Codec.Serialise (Serialise)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Concurrently(..))
import qualified Control.Concurrent.Async as Async
import           Control.Exception.Safe (Exception, MonadThrow)
import qualified Control.Exception.Safe as E
import           Control.Monad (unless, (>=>))
import           Control.Monad.Fix (mfix)
import           Control.Monad.IO.Unlift (MonadUnliftIO(..), wrappedWithRunInIO)
import           Data.Has (Has(..))
import           Data.Hashable (Hashable)
import           Data.Time.Clock (NominalDiffTime)
import           Lens.Micro (lens)
import           Network.Socket (HostName, PortNumber)
import qualified System.Random.SplitMix as SplitMix

data ProtocolMessage n =
      ProtocolBroadcast  (Plum.Message n)
    | ProtocolMembership (Hypa.RPC     n)
    deriving (Eq, Generic)

instance (Eq n, Hashable n, Serialise n) => Serialise (ProtocolMessage n)

data Handle e n o = Handle
    { hLogger     :: Logger
    , hIO         :: IO.Handle e (ProtocolMessage n) o
    , hBroadcast  :: Plum.Handle n
    , hSchedule   :: Plum.Schedule n
    , hMembership :: Hypa.Handle n
    }

instance IO.HasHandle (Handle e n o) e (ProtocolMessage n) o where
    handle = lens hIO (\s a -> s { hIO = a })
    {-# INLINE handle #-}

instance Plum.HasHandle (Handle e n o) n where
    handle = lens hBroadcast (\s a -> s { hBroadcast = a })
    {-# INLINE handle #-}

instance Hypa.HasHandle (Handle e n o) n where
    handle = lens hMembership (\s a -> s { hMembership = a })
    {-# INLINE handle #-}

instance Has Logger (Handle e n o) where
    hasLens = lens hLogger (\s a -> s { hLogger = a })
    {-# INLINE hasLens #-}

newtype GossipT e o m a = GossipT
    { unGossipT :: ReaderT (Handle e IO.Peer o) m a
    } deriving ( Functor
               , Applicative
               , Alternative
               , Monad
               , MonadIO
               , MonadReader (Handle e IO.Peer o)
               , MonadTrans
               , MonadMask
               , MonadCatch
               , MonadThrow
               )

instance MonadUnliftIO m => MonadUnliftIO (GossipT e o m) where
    withRunInIO = wrappedWithRunInIO GossipT unGossipT
    {-# INLINE withRunInIO #-}

instance MonadClock m => MonadClock (GossipT e o m)

type Gossip e o = GossipT e o IO
type Wire     = WireMessage (ProtocolMessage IO.Peer)

runGossipT :: Handle e IO.Peer o -> GossipT e o m a -> m a
runGossipT r (GossipT ma) = runReaderT ma r

withGossip
    :: ( Exception e
       , Serialise o
       )
    => Logger
    -> IO.Peer
    -> NominalDiffTime
    -> Plum.Callbacks
    -> Handshake e NodeId (WireMessage (ProtocolMessage IO.Peer)) o
    -> Hypa.Config
    -> (Handle e IO.Peer o -> IO a)
    -> IO a
withGossip lgr self sinterval storage handshake cfg k = do
    hdl <-
        mfix $ \hdl -> do
            let run = runGossipT hdl
            hio <-
                IO.new lgr handshake IO.Callbacks
                    { IO.recvPayload    = \sender -> run . dispatch sender
                    , IO.connectionLost = run . connectionLost
                    }
            hmb <- do
                gen <- SplitMix.initSMGen
                Hypa.new self cfg gen Hypa.Callbacks
                    { Hypa.neighborUp   = run . GossipT . Plum.neighborUp
                    , Hypa.neighborDown = run . GossipT . Plum.neighborDown
                    , Hypa.connOpen     = run . GossipT . IO.connect
                    , Hypa.connClose    = run . GossipT . IO.disconnect
                    }
            hbr <- Plum.new self storage
            sch <-
                Plum.newSchedule sinterval $ \to msg ->
                    run $ send to (WirePayload (ProtocolBroadcast msg))

            pure Handle
                { hLogger     = lgr
                , hIO         = hio
                , hBroadcast  = hbr
                , hSchedule   = sch
                , hMembership = hmb
                }

    -- FIXME(kim): factor into 'Membership'
    periodic <-
        Async.async . runConcurrently $
               (Concurrently $ do
                   threadDelay $ 10 * 1000000
                   runGossipT hdl $
                       GossipT Hypa.shuffle >>= traverse_ sendRPC)
            <> (Concurrently $ do
                   threadDelay $ 5 * 1000000
                   runGossipT hdl $
                       GossipT Hypa.promoteRandom >>= traverse_ sendRPC)

    k hdl
        `E.finally` Plum.destroySchedule (hSchedule hdl)
        `E.finally` Async.uninterruptibleCancel periodic

listen
    :: ( Exception e
       , Serialise o
       )
    => HostName
    -> PortNumber
    -> [Peer]
    -> Gossip e o Void
listen host port contacts =
    withRunInIO $ \runIO ->
        Async.withAsync (runIO ioListen) $ \lisn -> do
            runIO bootstrap
            Async.wait lisn
  where
    ioListen  = GossipT $ IO.listen host port
    bootstrap = do
        GossipT (Hypa.joinAny contacts) >>= traverse_ sendRPC
        GossipT $ Hypa.getPeers         >>= Plum.resetPeers

broadcast :: MonadIO m => Plum.MessageId -> ByteString -> GossipT e o m ()
broadcast mid msg =
    GossipT (Plum.broadcast mid msg) >>= traverse_ send'
  where
    send' out = asks hSchedule >>= liftIO . flip Plum.schedule out

--------------------------------------------------------------------------------

send :: IO.Peer -> WireMessage (ProtocolMessage IO.Peer) -> Gossip e o ()
send rcpt wire =
    GossipT (IO.send rcpt wire) `E.catchAny` const (connectionLost rcpt)

sendRPC :: Hypa.RPC IO.Peer -> Gossip e o ()
sendRPC rpc = send (Hypa.rpcRecipient rpc) (WirePayload (ProtocolMembership rpc))

connectionLost :: IO.Peer -> Gossip e o ()
connectionLost = GossipT . Hypa.eject >=> traverse_ sendRPC

dispatch :: IO.Peer -> ProtocolMessage IO.Peer -> Gossip e o ()
dispatch sender pm = do
    authorize authorized pm
    case pm of
        ProtocolBroadcast msg -> do
            sched <- asks hSchedule
            GossipT (Plum.receive msg) >>= traverse_ (liftIO . Plum.schedule sched)
        ProtocolMembership rpc ->
            GossipT (Hypa.receive rpc) >>= traverse_ sendRPC
  where
    authorized (ProtocolBroadcast msg) = case msg of
        Plum.IHaveM (Plum.IHave meta) -> Plum.metaSender meta == sender
        Plum.Prune from               -> from == sender
        Plum.Graft meta               -> Plum.metaSender meta == sender
        _                             -> True

    authorized (ProtocolMembership rpc) = case Hypa.rpcPayload rpc of
        Hypa.Shuffle{}     -> True
        Hypa.ForwardJoin{} -> True
        _                  -> Hypa.rpcSender rpc == sender

--------------------------------------------------------------------------------

data Unauthorized = Unauthorized
    deriving Show

instance Exception Unauthorized

authorize :: MonadThrow m => (a -> Bool) -> a -> m ()
authorize p a = unless (p a) $ E.throwM Unauthorized
