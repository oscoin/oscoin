module Oscoin.P2P.Handshake.Simple
    ( SimpleError (..)
    , keyExchange
    -- * Combinators
    , signedPayloads
    , whitelist
    , guardPeerId
    ) where

import           Oscoin.Prelude hiding (pi)

import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.P2P.Transport as Transport

import           Oscoin.P2P.Handshake.Types

import           Codec.Serialise (Serialise)
import           Control.Exception.Safe (Exception, throwM)

data HandshakeMessage =
      Hai Crypto.PublicKey
    -- ^ Present a pubkey and random value.
    | Bai SimpleError
    -- ^ At any point, either side may bail oout.
    deriving (Generic)

instance Serialise HandshakeMessage

data SimpleError =
      InvalidSignature
    | InvalidPayload
    | Unauthorized
    | IdMismatch
    | Timeout
    | Gone
    deriving (Eq, Show, Generic)

instance Exception SimpleError
instance Serialise SimpleError

-- | Exchanges the 'Crypto.PublicKey's of the participants.
--
-- Chained 'Handshake's will sign outgoing messages, and verify incoming ones.
--
-- **However**, the result's 'hrPreSend'/'hrPostRecv' will not - combine with
-- 'signedPayloads' for that. The reason for this is that, when chaining
-- 'Handshake's, it gives more flexibility wrt which parts of the payload are
-- signed, avoiding multiple signatures.
keyExchange :: Crypto.KeyPair -> Handshake SimpleError Crypto.PublicKey p p
keyExchange (myPK, mySK) _role peerId = do
    handshakeSend $ Hai myPK
    hai <- handshakeRecv mapRecvError
    case hai of
        Bai err     -> throwError err
        Hai theirPK -> do
            modifyTransport $
                Transport.framedEnvelope (sign mySK) (verify theirPK)
            guardPeerId peerId HandshakeResult
                { hrPeerId   = theirPK
                , hrPreSend  = idM
                , hrPostRecv = idM
                }
  where
    idM = pure . identity

-- Combinators -----------------------------------------------------------------

signedPayloads
    :: Serialise o
    => Crypto.PrivateKey
    -> HandshakeResult i o                 Crypto.PublicKey
    -> HandshakeResult i (Crypto.Signed o) Crypto.PublicKey
signedPayloads sk hr = mapOutput (sign sk) (verify (hrPeerId hr)) hr

-- | Transform a 'Handshake' with a lookup action for the obtained 'hrPeerId'.
--
-- If the 'hrPeerId' is not found by the lookup action (ie. it returns 'IO
-- False'), the handshake is aborted with 'Unauthorized'.
whitelist
    :: Eq n
    => (n -> IO Bool) -- ^ 'True' if this peer id is granted access, 'False' otherwise
    -> Handshake SimpleError n i o
    -> Handshake SimpleError n i o
whitelist acl f role peerId = do
    res <- f role peerId
    ok  <- lift $ acl (hrPeerId res)
    if ok then guardPeerId peerId res else throwError Unauthorized

--------------------------------------------------------------------------------

mapRecvError :: Transport.RecvError -> SimpleError
mapRecvError = \case
    Transport.RecvTimeout   -> Timeout
    Transport.RecvGarbage _ -> InvalidPayload
    Transport.RecvConnReset -> Gone

guardPeerId
    :: Eq n
    => Maybe n
    -> HandshakeResult i o n
    -> HandshakeT SimpleError IO (HandshakeResult i o n)
guardPeerId (Just peerId) res | peerId /= hrPeerId res = do
    handshakeSend $ Bai IdMismatch
    throwError IdMismatch

guardPeerId _ res = pure res

sign :: Serialise a => Crypto.PrivateKey -> a -> IO (Crypto.Signed a)
sign = Crypto.sign

verify :: Serialise a => Crypto.PublicKey -> Crypto.Signed a -> IO a
verify theirPK signed =
    let
        valid    = Crypto.verify theirPK signed
        unsigned = Crypto.unsign signed
     in
        bool (throwM InvalidSignature) (pure unsigned) valid
