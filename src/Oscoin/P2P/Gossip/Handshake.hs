{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Oscoin.P2P.Gossip.Handshake
    ( Handshake (..)

    -- * Simple (Insecure) Handshake
    , HandshakeError (..)
    , simple
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.P2P.Gossip.Connection (Connection(..), RecvError(..))
import qualified Oscoin.P2P.Gossip.Connection as Conn
import           Oscoin.P2P.Types (mkNodeId)

import           Codec.Serialise (Serialise)
import           Control.Concurrent.MVar (newMVar, withMVar)
import           Control.Exception.Safe (Exception, throwM)
import           Data.Bitraversable (bitraverse)
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as Conduit
import           Data.Conduit.Serialise (conduitDecodeCBOR)
import           Network.Socket (SockAddr, Socket)
import qualified Network.Socket as Sock (close)

type Handshake' e p =
       (Crypto.PublicKey, Crypto.PrivateKey)
    -> Socket
    -> SockAddr
    -> IO (Either e (Connection p))

data Handshake e p = Handshake
    { handshakeAcceptor  :: Handshake' e p
    , handshakeInitiator :: Handshake' e p
    }

simple :: Serialise p => Handshake HandshakeError p
simple = Handshake{..}
  where
    handshakeAcceptor  = simpleHandshake
    handshakeInitiator = simpleHandshake

data HandshakeMessage =
      Hai Crypto.PublicKey
    -- ^ Present a pubkey and random value.
    | Bai HandshakeError
    -- ^ At any point, either side may bail oout.
    deriving (Generic)

instance Serialise HandshakeMessage

data HandshakeError =
      InvalidSignature
    | InvalidPayload
    | Timeout
    | Gone
    deriving (Show, Generic)

instance Exception HandshakeError
instance Serialise HandshakeError

-- | A simple handshake which just exchanges the 'Crypto.PublicKey's of the
-- participants.
--
-- Subsequent protocol messages are signed when sending, and the signature is
-- verified when receiving.
--
-- This is not a secure protocol, as all communication is in clear text.
simpleHandshake
    :: Serialise p
    => (Crypto.PublicKey, Crypto.PrivateKey)
    -> Socket
    -> SockAddr
    -> IO (Either HandshakeError (Connection p))
simpleHandshake (myPK, mySK) sock addr = do
    conn <-
        runExceptT $ do
            sendE sock $ Hai myPK
            hai <- recvE sock
            case hai of
                Hai theirPK -> liftIO $ mkConnection theirPK mySK sock addr
                Bai err     -> throwE err

    bitraverse (\e -> bai sock e $> e) pure conn

--------------------------------------------------------------------------------

mkConnection
    :: Serialise p
    => Crypto.PublicKey
    -> Crypto.PrivateKey
    -> Socket
    -> SockAddr
    -> IO (Connection p)
mkConnection theirPK mySK sock addr = do
    mutex <- newMVar ()
    pure Connection
        { connNodeId   = mkNodeId theirPK
        , connAddr     = addr
        , connClose    = Sock.close sock
        , connSendWire = sendWire mutex
        , connRecvWire = recvWire
        }
  where
    sendWire mutex wire = do
        signed <- Crypto.sign mySK wire
        withMVar mutex . const $
            Conn.sockSendStream sock signed

    recvWire =
           Conn.sockRecvStream sock
        .| Conduit.mapM verify
        .| Conduit.map LBS.fromStrict
        .| conduitDecodeCBOR

    verify signed =
        let valid    = Crypto.verify theirPK signed
            unsigned = Crypto.unsign signed
         in bool (throwM InvalidSignature) (pure unsigned) valid

--------------------------------------------------------------------------------

recvE :: Socket -> ExceptT HandshakeError IO HandshakeMessage
recvE sock = withExceptT mapRecvError $ ExceptT recv
  where
    mapRecvError = \case
        RecvTimeout   -> Timeout
        RecvGarbage _ -> InvalidPayload
        RecvConnReset -> Gone

    recv = Conn.sockRecvFramed sock

sendE :: Socket -> HandshakeMessage -> ExceptT e IO ()
sendE sock msg = ExceptT $ map pure (Conn.sockSendFramed sock msg)

bai :: Socket -> HandshakeError -> IO ()
bai sock e = Conn.sockSendFramed sock $ Bai e
