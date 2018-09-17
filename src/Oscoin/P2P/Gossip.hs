module Oscoin.P2P.Gossip
    ( Gossip
    , Wire

    , withGossip

    , listen
    , broadcast
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.PubKey (PrivateKey, PublicKey)
import           Oscoin.Logging (Logger)
import qualified Oscoin.P2P.Gossip.Broadcast as Plum
import qualified Oscoin.P2P.Gossip.Handshake as Handshake
import qualified Oscoin.P2P.Gossip.IO as IO
import qualified Oscoin.P2P.Gossip.Membership as Hypa
import           Oscoin.P2P.Gossip.Wire

import           Codec.Serialise (Serialise)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Concurrently(..))
import qualified Control.Concurrent.Async as Async
import           Control.Exception.Safe (Exception, MonadThrow)
import qualified Control.Exception.Safe as E
import           Control.Monad (unless, (>=>))
import           Control.Monad.Fix (mfix)
import           Control.Monad.IO.Unlift (withRunInIO)
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

type Gossip e = ReaderT (Handle e IO.Peer) IO
type Wire     = WireMessage (ProtocolMessage IO.Peer)

data Handle e n = Handle
    { hLogger     :: Logger
    , hIO         :: IO.Handle e (ProtocolMessage n)
    , hBroadcast  :: Plum.Handle n
    , hSchedule   :: Plum.Schedule n
    , hMembership :: Hypa.Handle n
    }

instance IO.HasHandle (Handle e n) e (ProtocolMessage n) where
    handle = lens hIO (\s a -> s { hIO = a })
    {-# INLINE handle #-}

instance Plum.HasHandle (Handle e n) n where
    handle = lens hBroadcast (\s a -> s { hBroadcast = a })
    {-# INLINE handle #-}

instance Hypa.HasHandle (Handle e n) n where
    handle = lens hMembership (\s a -> s { hMembership = a })
    {-# INLINE handle #-}

instance Has Logger (Handle e n) where
    hasLens = lens hLogger (\s a -> s { hLogger = a })
    {-# INLINE hasLens #-}

withGossip
    :: Exception e
    => Logger
    -> (PublicKey, PrivateKey)
    -> IO.Peer
    -> NominalDiffTime
    -> Plum.Callbacks
    -> Handshake.Handshake e (ProtocolMessage IO.Peer)
    -> Hypa.Config
    -> Gossip e a
    -> IO a
withGossip lgr keys self sinterval storage handshake cfg k = do
    hdl <-
        mfix $ \hdl -> do
            let run = flip runReaderT hdl
            hio <-
                IO.new lgr keys handshake IO.Callbacks
                    { IO.recvPayload    = \sender -> run . dispatch sender
                    , IO.connectionLost = run . connectionLost
                    }
            hmb <- do
                gen <- SplitMix.initSMGen
                Hypa.new self cfg gen Hypa.Callbacks
                    { Hypa.neighborUp   = run . Plum.neighborUp
                    , Hypa.neighborDown = run . Plum.neighborDown
                    , Hypa.connOpen     = run . IO.connect
                    , Hypa.connClose    = run . IO.disconnect
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
                   flip runReaderT hdl $
                       Hypa.shuffle >>= traverse_ sendRPC)
            <> (Concurrently $ do
                    threadDelay $ 5 * 1000000
                    flip runReaderT hdl $
                        Hypa.promoteRandom >>= traverse_ sendRPC)

    runReaderT k hdl
        `E.finally` Plum.destroySchedule (hSchedule hdl)
        `E.finally` Async.uninterruptibleCancel periodic

listen :: Exception e => HostName -> PortNumber -> [IO.Peer] -> Gossip e ()
listen host port contacts = withRunInIO $ \runIO ->
    Async.withAsync (runIO ioListen) $ \lisn -> do
        runIO bootstrap
        Async.wait lisn
  where
    ioListen  = IO.listen host port
    bootstrap = do
        Hypa.joinAny contacts >>= traverse_ sendRPC
        Hypa.getPeers         >>= Plum.resetPeers

broadcast :: Plum.MessageId -> ByteString -> Gossip e ()
broadcast mid msg = Plum.broadcast mid msg >>= traverse_ send'
  where
    send' out = asks hSchedule >>= io . flip Plum.schedule out

--------------------------------------------------------------------------------

send
    :: ( IO.HasHandle   r e (ProtocolMessage IO.Peer)
       , Hypa.HasHandle r IO.Peer
       )
    => IO.Peer
    -> WireMessage (ProtocolMessage IO.Peer)
    -> ReaderT r IO ()
send rcpt wire = IO.send rcpt wire `E.catchAny` const (connectionLost rcpt)

sendRPC
    :: ( Hypa.HasHandle r IO.Peer
       , IO.HasHandle   r e (ProtocolMessage IO.Peer)
       )
    => Hypa.RPC IO.Peer
    -> ReaderT r IO ()
sendRPC rpc = send (Hypa.rpcRecipient rpc) (WirePayload (ProtocolMembership rpc))

connectionLost
    :: ( Hypa.HasHandle r IO.Peer
       , IO.HasHandle   r e (ProtocolMessage IO.Peer)
       )
    => IO.Peer
    -> ReaderT r IO ()
connectionLost = Hypa.eject >=> traverse_ sendRPC

dispatch :: IO.Peer -> ProtocolMessage IO.Peer -> Gossip e ()
dispatch sender pm = do
    authorize authorized pm
    case pm of
        ProtocolBroadcast msg -> do
            sched <- asks hSchedule
            Plum.receive msg >>= traverse_ (io . Plum.schedule sched)
        ProtocolMembership rpc ->
            Hypa.receive rpc >>= traverse_ sendRPC
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
