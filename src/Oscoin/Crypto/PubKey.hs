{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Crypto.PubKey
    ( Signed
    , sigMessage
    , sigSignature
    , Signature
    , PublicKey
    , publicKeyHash
    , PrivateKey
    , serialisePrivateKey
    , deserialisePrivateKey

    , KeyPair
    , generateKeyPair

    , sign
    , signBytes
    , signed
    , unsign
    , verify
    , verifyBytes
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Crypto.PubKey.ECC.Generate (generate)
import           Crypto.PubKey.ECC.Types (CurveName(SEC_p256k1), getCurveByName)
import qualified Crypto.PubKey.ECC.Types as ECC
import           Crypto.Random.Types (MonadRandom)

import qualified Codec.CBOR.Write as CBOR (toLazyByteString)
import qualified Codec.CBOR.Read as CBOR (deserialiseFromBytes)
import           Codec.Serialise (Serialise(..))
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Codec.Serialise.JSON (deserialiseParseJSON, serialiseToJSON)
import           Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base64.Extended as Base64
import qualified Data.ByteString.Lazy as LBS
import           Data.Text.Prettyprint.Doc
import           Web.HttpApiData

data PublicKey = PublicKey ECDSA.PublicKey (Crypto.Hashed ECDSA.PublicKey)
    deriving (Show, Generic)

mkPublicKey :: ECDSA.PublicKey -> PublicKey
mkPublicKey pk = PublicKey pk (Crypto.hash pk)

instance Serialise PublicKey where
    encode (PublicKey pk _) = encode pk
    decode = mkPublicKey <$> decode

instance ToJSON PublicKey where
    toJSON = serialiseToJSON

instance FromJSON PublicKey where
    parseJSON = deserialiseParseJSON

instance Crypto.Hashable PublicKey where
    hash (PublicKey _ h) = Crypto.toHashed (Crypto.fromHashed h)

deriving instance Generic ECDSA.PublicKey
instance Serialise ECDSA.PublicKey

deriving instance Generic ECC.Curve
instance Serialise ECC.Curve

deriving instance Generic ECC.Point
instance Serialise ECC.Point

deriving instance Generic ECC.CurveBinary
instance Serialise ECC.CurveBinary

deriving instance Generic ECC.CurvePrime
instance Serialise ECC.CurvePrime

deriving instance Generic ECC.CurveCommon
instance Serialise ECC.CurveCommon

instance Eq PublicKey where
    (==) (PublicKey _ h) (PublicKey _ h') = h == h'

instance Ord PublicKey where
    (<=) (PublicKey _ h) (PublicKey _ h') = h <= h'

publicKeyHash :: PublicKey -> Crypto.Hashed ECDSA.PublicKey
publicKeyHash (PublicKey _ h) = h

newtype PrivateKey = PrivateKey ECDSA.PrivateKey
    deriving (Show, Eq)


serialisePrivateKey :: PrivateKey -> LBS.ByteString
serialisePrivateKey (PrivateKey sk) =
    CBOR.toLazyByteString $
       CBOR.encodeListLen 3
    <> CBOR.encodeWord 0
    <> encode (ECDSA.private_curve sk)
    <> CBOR.encodeInteger (ECDSA.private_d sk)

deserialisePrivateKey :: LBS.ByteString -> Either CBOR.DeserialiseFailure PrivateKey
deserialisePrivateKey bs = second snd $ CBOR.deserialiseFromBytes decoder bs
  where
    decoder = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (3, 0) ->
                PrivateKey <$> (ECDSA.PrivateKey <$> decode <*> CBOR.decodeInteger)
            _ -> fail "CBOR: Invalid PrivateKey"

--------------------------------------------------------------------------------

newtype Signature = Signature ECDSA.Signature
    deriving (Eq, Show)

instance Ord Signature where
    (Signature a) <= (Signature b) =
        (ECDSA.sign_r a, ECDSA.sign_s a) <= (ECDSA.sign_r b, ECDSA.sign_s b)

instance ToJSON Signature where
    toJSON = serialiseToJSON

instance FromJSON Signature where
    parseJSON = deserialiseParseJSON

instance FromHttpApiData Signature where
    parseQueryParam _txt = notImplemented

instance Serialise Signature where
    encode (Signature ecdsa) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 0
        <> CBOR.encodeInteger (ECDSA.sign_r ecdsa)
        <> CBOR.encodeInteger (ECDSA.sign_s ecdsa)

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (3, 0) -> do
                ecdsa <-
                    liftA2 ECDSA.Signature CBOR.decodeInteger CBOR.decodeInteger
                pure $ Signature ecdsa

            _ -> fail "CBOR Signature: invalid ECDSA signature"

-- | A signed message.
-- Create these with "sign" and verify them with "verify".
data Signed msg = Signed { sigMessage :: msg, sigSignature :: Signature }
    deriving (Show, Eq, Ord, Functor, Generic)

instance Serialise msg => Serialise (Signed msg)

instance Crypto.Hashable msg => Crypto.Hashable (Signed msg) where
    hash :: Signed msg -> Crypto.Hashed (Signed msg)
    hash (Signed msg _) = Crypto.toHashed (Crypto.fromHashed (Crypto.hash msg))

instance ToJSON (Signed ByteString) where
    toJSON (Signed msg sig) =
        object [ "msg" .= toJSON (Base64.encode msg)
               , "sig" .= toJSON sig
               ]

instance FromJSON (Signed ByteString) where
    parseJSON = withObject "Signed Tx" $ \o -> do
        msg <- o .: "msg"
        sig <- o .: "sig"
        pure $ signed sig (Base64.decode msg)

instance Pretty msg => Pretty (Signed msg) where
    pretty = pretty . unsign


type KeyPair = (PublicKey, PrivateKey)


-- | Generate a new random keypair.
generateKeyPair :: MonadRandom m => m KeyPair
generateKeyPair = do
    (pk, sk) <- generate (getCurveByName SEC_p256k1)
    pure (PublicKey pk (Crypto.hash pk), PrivateKey sk)

-- | Sign a message with a private key.
sign :: (MonadRandom m, Serialise msg) => PrivateKey -> msg -> m (Signed msg)
sign key msg = do
    s <- signBytes key . LBS.toStrict $ CBOR.serialise msg
    pure $ s $> msg

signBytes :: MonadRandom m => PrivateKey -> ByteString -> m (Signed ByteString)
signBytes (PrivateKey key) bytes =
    Signed bytes . Signature <$> ECDSA.sign key Crypto.hashAlgorithm bytes

-- | Create a signed message from a message and a signature.
signed :: Signature -> msg -> Signed msg
signed sig msg = Signed msg sig

-- | Unwrap a signed message from its signature.
unsign :: Signed msg -> msg
unsign = sigMessage

-- | Verify a signed message with the public key.
verify :: Serialise msg => PublicKey -> Signed msg -> Bool
verify key s = verifyBytes key $ map (LBS.toStrict . CBOR.serialise) s

verifyBytes :: PublicKey -> Signed ByteString -> Bool
verifyBytes (PublicKey key _) (Signed bytes (Signature sig)) =
    ECDSA.verify Crypto.hashAlgorithm key sig bytes
