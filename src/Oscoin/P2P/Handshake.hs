module Oscoin.P2P.Handshake
    ( simpleHandshake
    , secureHandshake

    , module Oscoin.P2P.Handshake.Simple
    , module Oscoin.P2P.Handshake.Noise

    , Types.Handshake
    , Types.HandshakeRole(..)
    , Types.HandshakeResult
    , Types.hrPeerId
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
import           Oscoin.P2P.Types (NodeId, fromNodeId, mkNodeId)

import           Codec.Serialise (Serialise)

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
    :: Serialise p
    => Crypto.KeyPair
    -> Handshake SimpleError NodeId p (Crypto.Signed p)
simpleHandshake keys role peerId = do
    res <- map mkNodeId . signedPayloads (snd keys)
       <$> keyExchange keys role Nothing
    guardPeerId peerId res

-- | A secure handshake combining 'noiseNNHandshake' and 'keyExchange'.
--
-- Subsequent protocol messages are encrypted and carry a signed
-- 'NoiseHandshakeHash', which is verified on reception. This is called
-- \"Channel Binding\" in the Noise spec (Section 11.2).
secureHandshake
    :: Serialise p
    => Crypto.KeyPair
    -> Handshake HandshakeError
                 NodeId
                 p
                 (NoisePayload (Crypto.Signed NoiseHandshakeHash, p))
secureHandshake keys role (map fromNodeId -> peer) = do
    noise <- withHandshakeT NoiseHandshakeError  $ noiseNNHandshake role Nothing
    keyex <- withHandshakeT SimpleHandshakeError $ keyExchange keys role peer
    let
        clear     = signedHandshakeHash (snd keys) $ map (,hrPeerId noise) keyex
        sendClear = hrPreSend clear
        recvClear = hrPostRecv clear
     in
        pure
            . map (const . mkNodeId $ hrPeerId clear)
            $ mapInput sendClear recvClear noise

-- Internal --------------------------------------------------------------------

signedHandshakeHash
    :: Crypto.PrivateKey
    -> HandshakeResult i o                                     (Crypto.PublicKey, NoiseHandshakeHash)
    -> HandshakeResult i (Crypto.Signed NoiseHandshakeHash, o) Crypto.PublicKey
signedHandshakeHash sk hr = map fst $ mapOutput send recv hr
  where
    (theirPK, handhakeHash) = hrPeerId hr

    send p = (,p) <$> Crypto.sign sk handhakeHash

    recv (s,p)
        | Crypto.verify theirPK s
       && Crypto.unsign s == handhakeHash = pure p
        | otherwise                       = throwM InvalidSignature
