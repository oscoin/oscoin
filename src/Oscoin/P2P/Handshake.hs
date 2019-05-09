module Oscoin.P2P.Handshake
    ( HandshakeError(..)

    , simpleHandshake
    , secureHandshake

    , module Oscoin.P2P.Handshake.Simple
    , module Oscoin.P2P.Handshake.Noise

    , Types.Handshake
    , Types.HandshakeRole(..)
    , Types.HandshakeResult
    , Types.hrPeerInfo
    , Types.hrPreSend
    , Types.hrPostRecv
    , Types.HandshakeT
    , Types.runHandshakeT
    , Types.withHandshakeT
    )
where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.PubKey as Crypto

import           Oscoin.P2P.Handshake.Noise
import           Oscoin.P2P.Handshake.Simple
import           Oscoin.P2P.Handshake.Types as Types
import           Oscoin.P2P.Types
                 ( Addr
                 , Network
                 , NodeInfo
                 , fromNodeId
                 , mkNodeId
                 , mkNodeInfo
                 , nodeNodeId
                 )

import           Codec.Serialise (Serialise)
import           Data.ByteArray (ByteArrayAccess)

data HandshakeError =
      SimpleHandshakeError SimpleError
    | NoiseHandshakeError  NoiseError
    deriving (Show, Generic)

instance Exception HandshakeError

-- | A simple handshake using 'keyExchange'.
--
-- Subsequent protocol messages are signed when sending, and the signature is
-- verified when receiving.
--
-- This is not a secure protocol, as all communication is in clear text.
simpleHandshake
    :: forall p c.
       ( Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , ByteArrayAccess p
       , Crypto.HasDigitalSignature c
       )
    => Crypto.KeyPair c
    -> Addr
    -> Network
    -> Handshake SimpleError (NodeInfo c) p (Crypto.Signed c p)
simpleHandshake keys myApiAddr net role peerId = do
    res <- map mkNodeId . signedPayloads (snd keys)
       <$> keyExchange keys net role Nothing
    inf <- infoExchange myApiAddr net role Nothing
    let
        proxy       = Proxy @(Crypto.PublicKey c)
        myPk        = let (pk,_) = keys in mkNodeId pk
        knownPeerId = map nodeNodeId peerId
        guards self selfId = guardPeerIdNot proxy self >=> guardPeerId proxy selfId
     in
        mapInfo (mkNodeInfo (hrPeerInfo inf)) <$> guards myPk knownPeerId res

-- | A secure handshake combining 'noiseNNHandshake' and 'keyExchange'.
--
-- Subsequent protocol messages are encrypted and carry a signed
-- 'NoiseHandshakeHash', which is verified on reception. This is called
-- \"Channel Binding\" in the Noise spec (Section 11.2).
secureHandshake
    :: forall p c.
       ( Serialise p
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , Crypto.HasDigitalSignature c
       )
    => Crypto.PrivateKey c
    -> NodeInfo c
    -> Network
    -> Handshake HandshakeError
                 (NodeInfo c)
                 p
                 (NoisePayload (Crypto.Signed c NoiseHandshakeHash, p))
secureHandshake mySK myInfo net role mbPeerInfo = do
    noise     <-
        withHandshakeT NoiseHandshakeError $ noiseNNHandshake role Nothing
    theirInfo <-
        withHandshakeT SimpleHandshakeError $
            infoExchange myInfo net role mbPeerInfo >>=
                guardPeerIdNot (Proxy @(Crypto.PublicKey c)) myInfo
    let
        clear     = signedHandshakeHash mySK $ map (,hrPeerInfo noise) theirInfo
        sendClear = hrPreSend clear
        recvClear = hrPostRecv clear
     in
        pure . map (const $ hrPeerInfo clear)
             $ mapInput sendClear recvClear noise

-- Internal --------------------------------------------------------------------

signedHandshakeHash
    :: Crypto.HasDigitalSignature c
    => Crypto.PrivateKey c
    -> HandshakeResult i o                                       (NodeInfo c, NoiseHandshakeHash)
    -> HandshakeResult i (Crypto.Signed c NoiseHandshakeHash, o) (NodeInfo c)
signedHandshakeHash sk hr = map fst $ mapOutput send recv hr
  where
    (theirPK, handhakeHash) = first (fromNodeId . nodeNodeId) $ hrPeerInfo hr

    send p = (,p) <$> Crypto.sign sk handhakeHash

    recv (s,p)
        | Crypto.verify theirPK s
       && Crypto.unsign s == handhakeHash = pure p
        | otherwise                       = throwM InvalidSignature
