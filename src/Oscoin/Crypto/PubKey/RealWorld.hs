{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Crypto.PubKey.RealWorld where

import           Oscoin.Prelude

import qualified Codec.CBOR.Read as CBOR (deserialiseFromBytes)
import qualified Codec.CBOR.Write as CBOR (toLazyByteString)
import           Codec.Serialise
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Codec.Serialise.JSON (deserialiseParseJSON, serialiseToJSON)
import           Control.Monad.Fail (fail)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Crypto.PubKey.ECC.Generate (generate)
import           Crypto.PubKey.ECC.Types (CurveName(SEC_p256k1), getCurveByName)
import           Data.Aeson hiding (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Hash.RealWorld ()
import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.PubKey.Internal (PrivateKey(..), PublicKey(..))

instance HasDigitalSignature Crypto where

    newtype PK Crypto =
        PK (PublicKey Crypto ECDSA.PublicKey) deriving (Show, Eq)

    newtype SK Crypto = SK (PrivateKey ECDSA.PrivateKey)

    newtype Signature Crypto =
        Signature ECDSA.Signature deriving (Show, Eq)

    sign (SK (PrivateKey sk)) bytes =
        let (alg :: HashAlgorithm Crypto) = hashAlgorithm
         in Signed bytes . Signature <$> ECDSA.sign sk alg bytes

    verify (PK (PublicKey pk _)) (Signed bytes (Signature sig)) =
        let (alg :: HashAlgorithm Crypto) = hashAlgorithm
         in ECDSA.verify alg pk sig bytes

    generateKeyPair = do
        (pk, sk) <- generate (getCurveByName SEC_p256k1)
        pure (PK $ PublicKey pk (hash pk), SK $ PrivateKey sk)


{------------------------------------------------------------------------------
  Various instances
-------------------------------------------------------------------------------}

instance Ord (Signature Crypto) where
    (Signature a) <= (Signature b) =
        (ECDSA.sign_r a, ECDSA.sign_s a) <= (ECDSA.sign_r b, ECDSA.sign_s b)

instance ToJSON (Signature Crypto) where
    toJSON = serialiseToJSON

instance FromJSON (Signature Crypto) where
    parseJSON = deserialiseParseJSON

instance Serialise (Signature Crypto) where
    encode (Signature ecdsa) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 0
        <> CBOR.encodeInteger (ECDSA.sign_r ecdsa)
        <> CBOR.encodeInteger (ECDSA.sign_s ecdsa)

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (3, 0) -> Signature <$>
                liftA2 ECDSA.Signature CBOR.decodeInteger CBOR.decodeInteger
            _ -> fail "CBOR Signature: invalid ECDSA signature"

instance Eq (PublicKey Crypto ECDSA.PublicKey) where
    (PublicKey a1 b1) == (PublicKey a2 b2) = a1 == a2 && b1 == b2

instance H.Hashable (PK Crypto) where
    hashWithSalt salt (PK (PublicKey _ h)) = H.hashWithSalt salt . fromHashed $ h

instance Serialise (PK Crypto) where
    encode (PK (PublicKey pk _)) = encode pk
    decode = (\pk -> PK $ PublicKey @Crypto pk (hash pk)) <$> decode

instance Hashable Crypto (PK Crypto) where
    hash (PK (PublicKey _ h)) = toHashed $ fromHashed h

instance ToJSON (PK Crypto) where
    toJSON = serialiseToJSON

instance FromJSON (PK Crypto) where
    parseJSON = deserialiseParseJSON


{------------------------------------------------------------------------------
  Utility functions
-------------------------------------------------------------------------------}

serialisePrivateKey :: SK Crypto -> LBS.ByteString
serialisePrivateKey (SK (PrivateKey sk)) =
    CBOR.toLazyByteString $
       CBOR.encodeListLen 3
    <> CBOR.encodeWord 0
    <> encode (ECDSA.private_curve sk)
    <> CBOR.encodeInteger (ECDSA.private_d sk)

deserialisePrivateKey
    :: LBS.ByteString
    -> Either CBOR.DeserialiseFailure (SK Crypto)
deserialisePrivateKey bs = second snd $ CBOR.deserialiseFromBytes decoder bs
  where
    decoder = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (3, 0) ->
                SK . PrivateKey <$> (ECDSA.PrivateKey <$> decode <*> CBOR.decodeInteger)
            _ -> fail "CBOR: Invalid PrivateKey"
