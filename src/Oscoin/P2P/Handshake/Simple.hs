{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Oscoin.P2P.Handshake.Simple
    ( -- * Simple (Insecure) Handshake
      HandshakeError (..)
    , simpleHandshake
    , keyExchange
    -- * Combinators
    , peerNodeId
    , signedPayloads
    , whitelist
    ) where

import           Oscoin.Prelude hiding (pi)

import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.P2P.Connection (RecvError(..), Socket')
import qualified Oscoin.P2P.Connection as Conn
import           Oscoin.P2P.Types (NodeId, mkNodeId)

import           Oscoin.P2P.Handshake.Types

import           Codec.Serialise (Serialise)
import           Control.Exception.Safe (Exception, throwM)
import           Data.Bitraversable (bitraverse)

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
    | Unauthorized
    | IdMismatch
    | Timeout
    | Gone
    deriving (Show, Generic)

instance Exception HandshakeError
instance Serialise HandshakeError

-- | A simple handshake using 'keyExchange'.
--
-- Subsequent protocol messages are signed when sending, and the signature is
-- verified when receiving.
--
-- This is not a secure protocol, as all communication is in clear text.
simpleHandshake
    :: Serialise p
    => Crypto.KeyPair
    -> Handshake HandshakeError NodeId p (Crypto.Signed p)
simpleHandshake keys@(_, mySK) role sock peerId = do
    res <-
        runExceptT $ do
            res <-
                ExceptT $
                    map (peerNodeId . signedPayloads mySK)
                <$> keyExchange keys role sock Nothing
            matchPeerIdE peerId res
    conclude sock res

-- | Just exchanges the 'Crypto.PublicKey's of the participants.
keyExchange :: Crypto.KeyPair -> Handshake HandshakeError Crypto.PublicKey p p
keyExchange (myPK, _) _role sock peerId = do
    res <-
        runExceptT $ do
            sendE sock $ Hai myPK
            hai <- recvE sock
            case hai of
                Hai theirPK -> let res = HandshakeResult theirPK idM idM
                                in matchPeerIdE peerId res
                Bai err     -> throwE err
    conclude sock res

-- Combinators -----------------------------------------------------------------

peerNodeId :: HandshakeResult Crypto.PublicKey i o -> HandshakeResult NodeId i o
peerNodeId = mapHandshakeResult mkNodeId idM idM

signedPayloads
    :: Serialise o
    => Crypto.PrivateKey
    -> HandshakeResult Crypto.PublicKey i o
    -> HandshakeResult Crypto.PublicKey i (Crypto.Signed o)
signedPayloads mySK hr =
    mapHandshakeResult identity (Crypto.sign mySK) (verify (hrPeerId hr)) hr
  where
    verify theirPK signed =
        let valid    = Crypto.verify theirPK signed
            unsigned = Crypto.unsign signed
         in bool (throwM InvalidSignature) (pure unsigned) valid

-- | Transform a 'Handshake' with a lookup action for the obtained 'hrPeerId'.
--
-- If the 'hrPeerId' is not found by the lookup action (ie. it returns 'IO
-- False'), the handshake is aborted with 'Unauthorized'.
--
-- Note that this transforms a 'Handshake', not a 'HandshakeResult', so that the
-- error is raised early (vs. on the first protocol message).
whitelist
    :: (n -> IO Bool) -- ^ 'True' if this peer id is granted access, 'False' otherwise
    -> Handshake HandshakeError n i o
    -> Handshake HandshakeError n i o
whitelist acl f role sock peerId = do
    res <-
        runExceptT $ do
            res <- ExceptT $ f role sock peerId
            ok  <- lift $ acl (hrPeerId res)
            if ok then pure res else throwE Unauthorized
    conclude sock res

--------------------------------------------------------------------------------

recvE :: Socket' -> ExceptT HandshakeError IO HandshakeMessage
recvE conn = withExceptT mapRecvError $ ExceptT (Conn.framedRecv conn)
  where
    mapRecvError = \case
        RecvTimeout   -> Timeout
        RecvGarbage _ -> InvalidPayload
        RecvConnReset -> Gone

sendE :: Socket' -> HandshakeMessage -> ExceptT e IO ()
sendE conn msg = lift $ Conn.framedSend conn msg

matchPeerIdE
    :: (Eq n, Monad m)
    => Maybe n
    -> HandshakeResult n i o
    -> ExceptT HandshakeError m (HandshakeResult n i o)
matchPeerIdE (Just pi) res | pi /= hrPeerId res = throwE IdMismatch
matchPeerIdE _         res = pure res

conclude :: Socket' -> Either HandshakeError a -> IO (Either HandshakeError a)
conclude sock = bitraverse (\e -> bai sock e $> e) pure

bai :: Socket' -> HandshakeError -> IO ()
bai conn e = Conn.framedSend conn $ Bai e

--------------------------------------------------------------------------------

idM :: Monad m => a -> m a
idM = pure . identity
