{-# LANGUAGE UndecidableInstances #-}
module Oscoin.P2P.Handshake.Simple
    ( SimpleError (..)
    , keyExchange
    -- * Combinators
    , signedPayloads
    , whitelist

    , guardResult
    , peerIdMatch
    , guardPeerId
    , guardPeerIdNot
    ) where

import           Oscoin.Prelude hiding (length, pi)

import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.P2P.Handshake.Types
import qualified Oscoin.P2P.Transport as Transport
import           Oscoin.P2P.Types (Network)

import           Codec.Serialise (Serialise)
import           Control.Exception.Safe (Exception, throwM)
import           Data.ByteArray (ByteArrayAccess)
import           Data.ByteArray.Orphans ()


data HandshakeMessage c =
      Hai (Crypto.PublicKey c) Network
    -- ^ Present a pubkey and network name.
    | Bai SimpleError
    -- ^ At any point, either side may bail out.
    deriving (Generic)

instance Serialise (Crypto.PublicKey c) => Serialise (HandshakeMessage c)

data SimpleError =
      InvalidSignature
    | InvalidPayload
    | Unauthorized
    | IdMismatch
    | NetworkMismatch
    | DuplicateId
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
keyExchange
    :: forall c p.
       ( Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , Eq (Crypto.PublicKey c)
       , Crypto.HasDigitalSignature c
       )
    => Crypto.KeyPair c
    -> Network
    -> Handshake SimpleError (Crypto.PublicKey c) p p
keyExchange (myPK, mySK) net _role peerId = do
    handshakeSend $ Hai myPK net
    hai <- handshakeRecv mapRecvError
    case hai of
        Bai err              -> throwError err
        Hai theirPK theirNet | net /= theirNet -> throwError NetworkMismatch
                             | otherwise       -> do
            modifyTransport $
                Transport.framedEnvelope (sign mySK) (verify theirPK)
            guardPeerId (Proxy @c) peerId  HandshakeResult
                { hrPeerId   = theirPK
                , hrPreSend  = idM
                , hrPostRecv = idM
                }
  where
    idM = pure . identity

-- Combinators -----------------------------------------------------------------

signedPayloads
    :: (ByteArrayAccess o, Crypto.HasDigitalSignature c)
    => Crypto.PrivateKey c
    -> HandshakeResult i o                   (Crypto.PublicKey c)
    -> HandshakeResult i (Crypto.Signed c o) (Crypto.PublicKey c)
signedPayloads sk hr = mapOutput (sign sk) (verify (hrPeerId hr)) hr

-- | Transform a 'Handshake' with a lookup action for the obtained 'hrPeerId'.
--
-- If the 'hrPeerId' is not found by the lookup action (ie. it returns 'IO
-- False'), the handshake is aborted with 'Unauthorized'.
whitelist
    :: forall i o n c proxy. (Eq n, Serialise (Crypto.PublicKey c))
    => proxy c
    -> (n -> IO Bool) -- ^ 'True' if this peer id is granted access, 'False' otherwise
    -> Handshake SimpleError n i o
    -> Handshake SimpleError n i o
whitelist p acl f role peerId = do
    res <- f role peerId
    ok  <- lift $ acl (hrPeerId res)
    if ok then guardPeerId p peerId res else throwError Unauthorized

--------------------------------------------------------------------------------

mapRecvError :: Transport.RecvError -> SimpleError
mapRecvError = \case
    Transport.RecvTimeout   -> Timeout
    Transport.RecvGarbage _ -> InvalidPayload
    Transport.RecvConnReset -> Gone

guardResult
    :: forall i o n c proxy. Serialise (Crypto.PublicKey c)
    => proxy c
    -> (HandshakeResult i o n -> Bool)
    -> SimpleError
    -> HandshakeResult i o n
    -> HandshakeT SimpleError IO (HandshakeResult i o n)
guardResult _ predicate e res
  | predicate res = handshakeSend (Bai e :: HandshakeMessage c) *> throwError e
  | otherwise     = pure res

peerIdMatch :: Eq n => n -> HandshakeResult i o n -> Bool
peerIdMatch peerId res = peerId == hrPeerId res

-- | If the given peer id is 'Just n', assert that 'n' is the same has
-- 'hrPeerId' of the 'HandshakeResult'.
guardPeerId
    :: forall i o n c proxy. (Eq n, Serialise (Crypto.PublicKey c))
    => proxy c
    -> Maybe n
    -> HandshakeResult i o n
    -> HandshakeT SimpleError IO (HandshakeResult i o n)
guardPeerId proxy peerId =
    guardResult proxy
                (maybe (const False) (\pid -> not . peerIdMatch pid) peerId)
                IdMismatch

-- | Assert that the given peer id 'n' is /not/ the same as 'hrPeerId' of the
-- 'HandshakeResult'.
guardPeerIdNot
    :: forall i o n c proxy. (Eq n, (Serialise (Crypto.PublicKey c)))
    => proxy c
    -> n
    -> HandshakeResult i o n
    -> HandshakeT SimpleError IO (HandshakeResult i o n)
guardPeerIdNot proxy peerId =
    guardResult proxy (peerIdMatch peerId) DuplicateId

sign
    :: (Crypto.HasDigitalSignature c, ByteArrayAccess a)
    => Crypto.PrivateKey c
    -> a
    -> IO (Crypto.Signed c a)
sign = Crypto.sign

verify
    :: (Crypto.HasDigitalSignature c, ByteArrayAccess a)
    => Crypto.PublicKey c
    -> Crypto.Signed c a
    -> IO a
verify theirPK signed =
    let
        valid    = Crypto.verify theirPK signed
        unsigned = Crypto.unsign signed
     in
        bool (throwM InvalidSignature) (pure unsigned) valid
