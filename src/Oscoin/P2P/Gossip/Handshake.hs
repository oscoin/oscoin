{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import qualified Codec.Serialise as CBOR
import           Control.Exception.Safe (Exception, throwM)
import           Control.Monad (unless)
import           Crypto.Random (getRandomBytes)
import           Data.Bitraversable (bitraverse)
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as Conduit
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
    handshakeAcceptor  = serverHandshake
    handshakeInitiator = clientHandshake

-- | A simplified authentication-only STS.
--
-- This is very obviously vulnerable to a man-in-the-middle attack. A proper
-- implementation would run a DH key exchange, and perform authentication
-- encrypted using the session key. Or, just run TLS.
data HandshakeMessage =
      Hai  Crypto.PublicKey ByteString
    -- ^ Present a pubkey and random value.
    | Ohai Crypto.PublicKey Crypto.Signature ByteString
    -- ^ Respond to 'Hai' with own pubkey, signature of @myR <> theirR@ , and
    -- own random value 'myR'.
    | Kk   Crypto.Signature
    -- ^ Respond to 'Ohai' with signature of @myR <> theirR@.
    | Thx
    -- ^ Conclude the protocol successfully.
    | Bai  HandshakeError
    -- ^ At any point, either side may bail oout.
    deriving (Generic)

instance Serialise HandshakeMessage

data HandshakeError =
      InvalidSignature
    | InvalidPayload
    | BadProtocolSequence
    | Timeout
    | Gone
    deriving (Show, Generic)

instance Exception HandshakeError
instance Serialise HandshakeError

serverHandshake
    :: Serialise p
    => (Crypto.PublicKey, Crypto.PrivateKey)
    -> Socket
    -> SockAddr
    -> IO (Either HandshakeError (Connection p))
serverHandshake (myPK, mySK) sock addr = do
    conn <-
        runExceptT $ do
            hai <- recvE sock
            case hai of
                Hai theirPK theirRnd -> do
                    myRnd <- randomE
                    mySig <- signE mySK (myRnd <> theirRnd)
                    sendE sock $ Ohai myPK mySig myRnd
                    kk    <- recvE sock
                    case kk of
                        Kk  theirSig -> do
                            verifyE theirPK theirSig (theirRnd <> myRnd)
                            sendE sock Thx
                            pure $ mkConnection theirPK mySK sock addr
                        Bai err      -> throwE err
                        _            -> throwE BadProtocolSequence

                _ -> throwE BadProtocolSequence

    bitraverse (\e -> bai sock e $> e) pure conn

clientHandshake
    :: Serialise p
    => (Crypto.PublicKey, Crypto.PrivateKey)
    -> Socket
    -> SockAddr
    -> IO (Either HandshakeError (Connection p))
clientHandshake (myPK, mySK) sock addr = do
    conn <-
        runExceptT $ do
            myRnd <- randomE
            sendE sock $ Hai myPK myRnd
            ohai  <- recvE sock
            case ohai of
                Ohai theirPK theirSig theirRnd -> do
                    verifyE theirPK theirSig (theirRnd <> myRnd)
                    mySig <- signE mySK (myRnd <> theirRnd)
                    sendE sock $ Kk mySig
                    thx   <- recvE sock
                    case thx of
                        Thx     -> pure $ mkConnection theirPK mySK sock addr
                        Bai err -> throwE err
                        _       -> throwE BadProtocolSequence

                _ -> throwE BadProtocolSequence

    bitraverse (\e -> bai sock e $> e) pure conn

--------------------------------------------------------------------------------

mkConnection
    :: Serialise p
    => Crypto.PublicKey
    -> Crypto.PrivateKey
    -> Socket
    -> SockAddr
    -> Connection p
mkConnection theirPK mySK sock connAddr =
    Connection {..}
  where
    connNodeId        = mkNodeId theirPK
    connClose         = Sock.close sock
    connSendWire wire = sign wire >>= Conn.sockSendStream sock
    connRecvWire      =
           Conn.sockRecvStream sock
        .| Conduit.mapM verify
        .| Conduit.map LBS.fromStrict
        .| Conn.conduitDecodeCBOR

    verify signed =
        let valid    = Crypto.verifyBytes theirPK signed
            unsigned = Crypto.unsign signed
         in bool (throwM InvalidSignature) (pure unsigned) valid

    sign wire = Crypto.signBytes mySK . LBS.toStrict $ CBOR.serialise wire

--------------------------------------------------------------------------------

recvE :: Socket -> ExceptT HandshakeError IO HandshakeMessage
recvE sock = withExceptT mapRecvError $ ExceptT recv
  where
    mapRecvError = \case
        RecvTimeout   -> Timeout
        RecvGarbage _ -> InvalidPayload
        RecvConnReset -> Gone

    recv = Conn.sockRecvFramed sock

sendE :: Socket -> HandshakeMessage -> ExceptT HandshakeError IO ()
sendE sock msg = ExceptT $ map pure (Conn.sockSendFramed sock msg)

signE
    :: Crypto.PrivateKey
    -> ByteString
    -> ExceptT HandshakeError IO Crypto.Signature
signE mySK msg = map Crypto.sigSignature . liftIO $ Crypto.sign mySK msg

verifyE
    :: Crypto.PublicKey
    -> Crypto.Signature
    -> ByteString
    -> ExceptT HandshakeError IO ()
verifyE theirPK theirSig myRnd =
    unless (Crypto.verify theirPK (Crypto.signed theirSig myRnd)) $
        throwE InvalidSignature

randomE :: ExceptT HandshakeError IO ByteString
randomE = liftIO (getRandomBytes 32)

bai :: Socket -> HandshakeError -> IO ()
bai sock e = Conn.sockSendFramed sock $ Bai e
