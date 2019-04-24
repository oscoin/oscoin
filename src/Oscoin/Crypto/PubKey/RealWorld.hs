{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Crypto.PubKey.RealWorld where

import           Oscoin.Prelude hiding (length)

import qualified Codec.CBOR.Read as CBOR (deserialiseFromBytes)
import qualified Codec.CBOR.Write as CBOR (toLazyByteString)
import           Codec.Serialise
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Codec.Serialise.JSON (deserialiseParseJSON, serialiseToJSON)
import           Control.Monad.Fail (fail)
import           Crypto.Error (eitherCryptoError)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Data.Aeson hiding (decode, encode)
import           Data.ByteArray (ByteArrayAccess(..), convert)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Hash.RealWorld ()
import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.PubKey.Internal (PK(..), SK(..))

instance HasDigitalSignature Crypto where

    newtype PublicKey Crypto =
        PublicKey (PK Crypto Ed25519.PublicKey) deriving (Show, Eq, Ord)

    newtype PrivateKey Crypto = PrivateKey (SK Ed25519.SecretKey)

    newtype Signature Crypto =
        Signature Ed25519.Signature deriving Show

    sign (PrivateKey (SK sk)) bytes =
        pure $ Signed bytes . Signature . Ed25519.sign sk (Ed25519.toPublic sk) $ bytes

    verify (PublicKey (PK pk _)) (Signed bytes (Signature sig)) =
        Ed25519.verify pk bytes sig

    generateKeyPair = do
        sk <- Ed25519.generateSecretKey
        let pk = Ed25519.toPublic sk
        pure (PublicKey $ PK pk (toHashed . hashByteArray $ pk), PrivateKey $ SK sk)

instance Eq (Signature Crypto) where
    (Signature s1) == (Signature s2) = s1 == s2

{------------------------------------------------------------------------------
  Various instances
-------------------------------------------------------------------------------}

instance Ord (Signature Crypto) where
    (Signature a) <= (Signature b) =
        BA.convert @_ @ByteString a <= BA.convert @_ @ByteString b

instance ToJSON (Signature Crypto) where
    toJSON = serialiseToJSON

instance FromJSON (Signature Crypto) where
    parseJSON = deserialiseParseJSON

instance Serialise (Signature Crypto) where
    encode (Signature ed25519) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encodeBytes (BA.convert ed25519)

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (2, 0) -> do
                sigE <- Ed25519.signature <$> CBOR.decodeBytes
                case eitherCryptoError sigE of
                  Left e -> fail ("CBOR signature conversion failed: " ++ show e)
                  Right s -> pure $ Signature s
            _      -> fail "CBOR Signature: invalid ECDSA signature"

instance Eq (PK Crypto Ed25519.PublicKey) where
    (PK a1 b1) == (PK a2 b2) = a1 == a2 && b1 == b2

instance Ord (PK Crypto Ed25519.PublicKey) where
    compare (PK _ b1) (PK _ b2) = b1 `compare` b2

instance H.Hashable (PublicKey Crypto) where
    hashWithSalt salt (PublicKey (PK _ h)) = H.hashWithSalt salt . fromHashed $ h

instance ByteArrayAccess (PublicKey Crypto) where
    length             = fromIntegral . LBS.length . serialise
    withByteArray pk f = withByteArray (LBS.toStrict (serialise pk)) f

instance Serialise (PublicKey Crypto) where
    encode (PublicKey (PK pk _)) = CBOR.encodeBytes (BA.convert pk)
    decode = do
        pkE <- Ed25519.publicKey <$> CBOR.decodeBytes
        case eitherCryptoError pkE of
            Left e   -> fail ("CBOR PublicKey conversion failed: " ++ show e)
            Right pk -> pure $ PublicKey $ PK @Crypto pk (toHashed . hashByteArray $ pk)

instance Hashable Crypto (PublicKey Crypto) where
    hash (PublicKey (PK _ h)) = toHashed $ fromHashed h

instance ToJSON (PublicKey Crypto) where
    toJSON = serialiseToJSON

instance FromJSON (PublicKey Crypto) where
    parseJSON = deserialiseParseJSON


{------------------------------------------------------------------------------
  Utility functions
-------------------------------------------------------------------------------}

serialisePrivateKey :: PrivateKey Crypto -> LBS.ByteString
serialisePrivateKey (PrivateKey (SK sk)) =
    CBOR.toLazyByteString $
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 0
    <> CBOR.encodeBytes (BA.convert sk)

deserialisePrivateKey
    :: LBS.ByteString
    -> Either CBOR.DeserialiseFailure (PrivateKey Crypto)
deserialisePrivateKey bs = second snd $ CBOR.deserialiseFromBytes decoder bs
  where
    decoder = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (2, 0) -> do
                skE <- Ed25519.secretKey <$> CBOR.decodeBytes
                case eitherCryptoError skE of
                  Left e -> fail ("CBOR SecretKey conversion failed: " ++ show e)
                  Right sk -> pure . PrivateKey $ SK sk
            _ -> fail "CBOR: Invalid SK"
