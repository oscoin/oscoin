{-# LANGUAGE UndecidableInstances #-}
module Oscoin.P2P.Handshake.Simple
    ( SimpleError (..)
    , keyExchange
    -- * Combinators
    , signedPayloads
    , whitelist
    , guardPeerId
    ) where

import           Oscoin.Prelude hiding (length, pi)

import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.P2P.Transport as Transport

import           Oscoin.P2P.Handshake.Types

import           Codec.Serialise (Serialise)
import           Control.Exception.Safe (Exception, throwM)
import           Data.ByteArray (ByteArrayAccess)
import           Data.ByteArray.Orphans ()


data HandshakeMessage c =
      Hai (Crypto.PK c)
    -- ^ Present a pubkey and random value.
    | Bai SimpleError
    -- ^ At any point, either side may bail oout.
    deriving (Generic)

instance Serialise (Crypto.PK c) => Serialise (HandshakeMessage c)

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

-- | Exchanges the 'Crypto.PK's of the participants.
--
-- Chained 'Handshake's will sign outgoing messages, and verify incoming ones.
--
-- **However**, the result's 'hrPreSend'/'hrPostRecv' will not - combine with
-- 'signedPayloads' for that. The reason for this is that, when chaining
-- 'Handshake's, it gives more flexibility wrt which parts of the payload are
-- signed, avoiding multiple signatures.
keyExchange
    :: forall c p.
       ( Serialise (Crypto.PK c)
       , Serialise (Crypto.Signature c)
       , Eq (Crypto.PK c)
       , Crypto.HasDigitalSignature c
       )
    => Crypto.KeyPair c
    -> Handshake SimpleError (Crypto.PK c) p p
keyExchange (myPK, mySK) _role peerId = do
    handshakeSend $ Hai myPK
    hai <- handshakeRecv mapRecvError
    case hai of
        Bai err     -> throwError err
        Hai theirPK -> do
            modifyTransport $
                Transport.framedEnvelope (sign mySK) (verify theirPK)
            guardPeerId (Proxy :: Proxy c) peerId HandshakeResult
                { hrPeerId   = theirPK
                , hrPreSend  = idM
                , hrPostRecv = idM
                }
  where
    idM = pure . identity

-- Combinators -----------------------------------------------------------------

signedPayloads
    :: (ByteArrayAccess o, Crypto.HasDigitalSignature c)
    => Crypto.SK c
    -> HandshakeResult i o                   (Crypto.PK c)
    -> HandshakeResult i (Crypto.Signed c o) (Crypto.PK c)
signedPayloads sk hr = mapOutput (sign sk) (verify (hrPeerId hr)) hr

-- | Transform a 'Handshake' with a lookup action for the obtained 'hrPeerId'.
--
-- If the 'hrPeerId' is not found by the lookup action (ie. it returns 'IO
-- False'), the handshake is aborted with 'Unauthorized'.
whitelist
    :: (Eq n, Serialise (Crypto.PK c))
    => Proxy c
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

guardPeerId
    :: forall i o n c. (Eq n, Serialise (Crypto.PK c))
    => Proxy c
    -> Maybe n
    -> HandshakeResult i o n
    -> HandshakeT SimpleError IO (HandshakeResult i o n)
guardPeerId Proxy (Just peerId) res | peerId /= hrPeerId res = do
    handshakeSend (Bai IdMismatch :: HandshakeMessage c)
    throwError IdMismatch

guardPeerId _ _ res = pure res

sign
    :: (Crypto.HasDigitalSignature c, ByteArrayAccess a)
    => Crypto.SK c
    -> a
    -> IO (Crypto.Signed c a)
sign = Crypto.sign

verify
    :: (Crypto.HasDigitalSignature c, ByteArrayAccess a)
    => Crypto.PK c
    -> Crypto.Signed c a
    -> IO a
verify theirPK signed =
    let
        valid    = Crypto.verify theirPK signed
        unsigned = Crypto.unsign signed
     in
        bool (throwM InvalidSignature) (pure unsigned) valid
