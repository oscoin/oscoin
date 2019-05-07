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
                 , nodeHttpApiAddr
                 , nodeInfo2Id
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
        knownPeerId = map nodeInfo2Id peerId
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
    => Crypto.KeyPair c
    -> Addr
    -> Network
    -> Handshake HandshakeError
                 (NodeInfo c)
                 p
                 (NoisePayload (Crypto.Signed c NoiseHandshakeHash, p))
secureHandshake keys myInfo net role mbPeerInfo = do
    noise  <- withHandshakeT NoiseHandshakeError $ noiseNNHandshake role Nothing
    infoex <-
        withHandshakeT SimpleHandshakeError $ do
            pk   <- keyExchange  keys net role theirPk >>= guardPeerIdNot proxy self
            addr <- infoExchange myInfo net role theirInfo >>= guardPeerIdNot proxy myInfo
            pure $ map (hrPeerInfo addr,) pk
    let
        clear     = signedHandshakeHash (snd keys) $ map (,hrPeerInfo noise) infoex
        sendClear = hrPreSend clear
        recvClear = hrPostRecv clear
     in
        pure . map (const . uncurry mkNodeInfo . second mkNodeId $ hrPeerInfo clear)
             $ mapInput sendClear recvClear noise
  where
    self      = fst keys
    theirPk   = map (fromNodeId . nodeInfo2Id) mbPeerInfo
    theirInfo = map nodeHttpApiAddr mbPeerInfo
    proxy     = Proxy @(Crypto.PublicKey c)

-- Internal --------------------------------------------------------------------

signedHandshakeHash
    :: Crypto.HasDigitalSignature c
    => Crypto.PrivateKey c
    -> HandshakeResult i o                                       ((info, Crypto.PublicKey c), NoiseHandshakeHash)
    -> HandshakeResult i (Crypto.Signed c NoiseHandshakeHash, o)  (info, Crypto.PublicKey c)
signedHandshakeHash sk hr = map fst $ mapOutput send recv hr
  where
    ((_info, theirPK), handhakeHash) = hrPeerInfo hr

    send p = (,p) <$> Crypto.sign sk handhakeHash

    recv (s,p)
        | Crypto.verify theirPK s
       && Crypto.unsign s == handhakeHash = pure p
        | otherwise                       = throwM InvalidSignature
