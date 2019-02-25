-- | Secure handshakes based on the <https://noiseprotocol.org/noise.html Noise Protocol>
module Oscoin.P2P.Handshake.Noise
    ( NoisePayload
    , NoiseError (..)
    , NoiseHandshakeHash

    , noiseIKHandshake
    , noiseNNHandshake
    ) where

import           Oscoin.Prelude

import           Oscoin.P2P.Handshake.Types
import qualified Oscoin.P2P.Transport as Transport

import           Crypto.Noise
                 ( Cipher
                 , DH
                 , HandshakeOpts
                 , HandshakePattern
                 , HandshakeRole(..)
                 , Hash
                 , NoiseResult(..)
                 , NoiseState
                 , ScrubbedBytes
                 , defaultHandshakeOpts
                 , handshakeComplete
                 , handshakeHash
                 , noiseState
                 , readMessage
                 , remoteStaticKey
                 , setLocalEphemeral
                 , setLocalStatic
                 , setRemoteStatic
                 , writeMessage
                 )
import qualified Crypto.Noise as Noise
import           Crypto.Noise.Cipher (Plaintext)
import           Crypto.Noise.Cipher.ChaChaPoly1305 (ChaChaPoly1305)
import           Crypto.Noise.DH (KeyPair, PublicKey, dhGenKey, dhPubToBytes)
import           Crypto.Noise.DH.Curve25519 (Curve25519)
import           Crypto.Noise.HandshakePatterns (noiseIK, noiseNN)
import           Crypto.Noise.Hash.BLAKE2b (BLAKE2b)

import           Codec.Serialise (Serialise(..))
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef

newtype NoisePayload p = NP ScrubbedBytes

instance Serialise (NoisePayload p) where
    encode (NP sbs) = CBOR.encodeBytes $ ByteArray.convert sbs
    decode          = NP . ByteArray.convert <$> CBOR.decodeBytes

newtype NoiseHandshakeHash = NHH ScrubbedBytes
    deriving Eq

instance Serialise NoiseHandshakeHash where
    encode (NHH sbs) = CBOR.encodeBytes $ ByteArray.convert sbs
    decode           = NHH . ByteArray.convert <$> CBOR.decodeBytes

data NoiseError =
      ConfigurationError Text
    | KeyMismatch
    | MissingRemoteStaticKey
    | NoiseException SomeException
    | NetworkError Transport.RecvError
    deriving (Show, Generic)

instance Exception NoiseError

prologue :: Plaintext
prologue = "rhubarb"

hello :: ScrubbedBytes
hello = "ohai"

-- | 'Handshake' based on the Noise @IK@ pattern.
--
-- * @I@: the static key of the initiator is transmitted immediately
-- * @K@: the static key of the responder is known to the initiator
--
-- 'Oscoin.P2P.Handshake.Types.HandshakeRole' is mapped to 'Noise.HandshakeRole'
-- as:
--
-- @
--      role Acceptor  = ResponderRole
--      role Connector = InitiatorRole
-- @
noiseIKHandshake
    :: Serialise p
    => KeyPair Curve25519
    -> Handshake NoiseError (PublicKey Curve25519) p (NoisePayload p)
-- Connector -> InitiatorRole
noiseIKHandshake _     Connector Nothing        = throwError MissingRemoteStaticKey
noiseIKHandshake myKey Connector (Just theirPK) = do
    opt  <- lift $ mkHandshakeOpts myKey (Just theirPK) InitiatorRole
    let nst = mkNoiseState opt noiseIK
    nst' <- handshakeLoop IncompleteLocal (Just theirPK) nst
    case remoteStaticKey nst' of
        Nothing  -> throwError MissingRemoteStaticKey
        Just rsk | rsk /= theirPK -> throwError KeyMismatch
                 | otherwise      -> mkResult rsk dhPubToBytes nst'

-- Acceptor -> ResponderRole
noiseIKHandshake myKey Acceptor _ = do
    opt  <- lift $ mkHandshakeOpts myKey Nothing ResponderRole
    let nst = mkNoiseState opt noiseIK
    nst' <- handshakeLoop IncompleteRemote Nothing nst
    case remoteStaticKey nst' of
        Nothing  -> throwError MissingRemoteStaticKey
        Just rsk -> mkResult rsk dhPubToBytes nst'

-- | 'Handshake' based on the Noise @NN@ pattern.
--
-- No static keys are known to the participants, nor are they exchanged. After
-- the handshake completes, application level authentication may be performed
-- via <https://noiseprotocol.org/noise.html#channel-binding Channel Binding>.
--
-- The peer id parameter of 'Handshake' is ignored (obviously).
noiseNNHandshake
    :: Serialise p
    => Handshake NoiseError NoiseHandshakeHash p (NoisePayload p)
noiseNNHandshake role _peerId = do
    eph <- lift dhGenKey
    let opt = setLocalEphemeral (Just eph)
            $ defaultHandshakeOpts role' prologue
    let nst = mkNoiseState opt noiseNN
    nst' <- handshakeLoop hstate Nothing nst
    mkResult (NHH (handshakeHash nst')) (const "0") nst'
  where
    role' = case role of
        Acceptor  -> ResponderRole
        Connector -> InitiatorRole

    hstate = case role' of
        ResponderRole -> IncompleteRemote
        InitiatorRole -> IncompleteLocal

-- Internal --------------------------------------------------------------------

data NoiseHandshakeState = IncompleteLocal | IncompleteRemote

handshakeLoop
    :: (Cipher c, DH d, Hash h)
    => NoiseHandshakeState
    -> Maybe (PublicKey d)
    -> NoiseState c d h
    -> HandshakeT NoiseError IO (NoiseState c d h)
handshakeLoop nhs rstatic nst =
    case nhs of
        IncompleteLocal  -> writeLoop hello nst
        IncompleteRemote -> do
            NP rpayload <- handshakeRecv NetworkError
            readLoop rpayload nst
  where
    writeLoop msg nst' =
        case writeMessage msg nst' of
            NoiseResultMessage ciph nst'' -> do
                handshakeSend $ NP ciph
                bool (handshakeLoop IncompleteRemote rstatic nst'')
                     (pure nst'')
                     (handshakeComplete nst'')

            NoiseResultNeedPSK nst'' ->
                case rstatic of
                    Nothing  -> throwError $ ConfigurationError "Local: PSK demanded, but none given"
                    Just psk -> writeLoop (dhPubToBytes psk) nst''

            NoiseResultException e -> throwError $ NoiseException e

    readLoop msg nst' =
        case readMessage msg nst' of
            NoiseResultMessage plain nst''
                | handshakeComplete nst'' -> do
                    when (plain /= hello) $
                        throwError $ ConfigurationError "Remote: hello message differs"
                    pure nst''

                | otherwise -> handshakeLoop IncompleteLocal rstatic nst''

            NoiseResultNeedPSK nst'' ->
                case rstatic of
                    Nothing  -> throwError $ ConfigurationError "Remote: PSK demanded, but none given"
                    Just psk -> readLoop (dhPubToBytes psk) nst''

            NoiseResultException e -> throwError $ NoiseException e

mkResult
    :: (Serialise p, Cipher c, DH d, Hash h)
    => n
    -> (n -> ScrubbedBytes)
    -> NoiseState c d h
    -> HandshakeT e IO (HandshakeResult p (NoisePayload p) n)
mkResult peerId conv nst = do
    (ref,ref') <- liftIO $ liftA2 (,) (newIORef nst) (newIORef nst)

    modifyTransport $
        Transport.framedEnvelope (noiseSend peerId conv ref)
                                 (noiseRecv peerId conv ref')
    pure HandshakeResult
        { hrPeerId   = peerId
        , hrPreSend  = noiseSend peerId conv ref
        , hrPostRecv = noiseRecv peerId conv ref'
        }

noiseSend
    :: (Serialise p, Cipher c, DH d, Hash h)
    => psk
    -> (psk -> ScrubbedBytes)
    -> IORef (NoiseState c d h)
    -> p
    -> IO (NoisePayload p)
noiseSend psk convpsk ref p = do
    let clear = ByteArray.convert . LBS.toStrict . CBOR.serialise $ p
    either throwM pure
        =<< atomicModifyIORef' ref (writeMessage' clear)
  where
    writeMessage' clear nst =
        case writeMessage clear nst of
            NoiseResultMessage ciph nst' -> (nst', Right (NP ciph))
            NoiseResultNeedPSK      nst' -> writeMessage' (convpsk psk) nst'
            NoiseResultException e       -> (nst, Left e)

noiseRecv
    :: (Serialise p, Cipher c, DH d, Hash h)
    => psk
    -> (psk -> ScrubbedBytes)
    -> IORef (NoiseState c d h)
    -> NoisePayload p
    -> IO p
noiseRecv psk convpsk ref (NP p) = do
    clear <-
        runExceptT $ do
            clear <- ExceptT $ atomicModifyIORef' ref (readMessage' p)
            withExceptT toException $ deserialiseE clear
    either throwM pure clear
  where
    readMessage' ciph nst =
        case readMessage ciph nst of
            NoiseResultMessage clear nst' -> (nst', Right clear)
            NoiseResultNeedPSK       nst' -> readMessage' (convpsk psk) nst'
            NoiseResultException e        -> (nst, Left e)

    deserialiseE =
        ExceptT . pure . CBOR.deserialiseOrFail . LBS.fromStrict . ByteArray.convert

mkNoiseState
    :: HandshakeOpts Curve25519
    -> HandshakePattern
    -> NoiseState ChaChaPoly1305 Curve25519 BLAKE2b
mkNoiseState opt pat = noiseState opt pat

mkHandshakeOpts
    :: DH d
    => KeyPair d
    -> Maybe (PublicKey d)
    -> Noise.HandshakeRole
    -> IO (HandshakeOpts d)
mkHandshakeOpts mine theirs role = do
    eph <- dhGenKey
    pure
        . setLocalEphemeral (Just eph)
        . setLocalStatic    (Just mine)
        . setRemoteStatic   theirs
        $ defaultHandshakeOpts role prologue
