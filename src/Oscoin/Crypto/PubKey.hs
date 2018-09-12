{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Crypto.PubKey
    ( Signed(..)
    , PublicKey
    , publicKeyHash
    , PrivateKey
    , generateKeyPair
    , sign
    , signed
    , unsign
    , verify
    ) where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash (Hashed, toHashed, hashAlgorithm, Hashable(..), fromHashed, hash)

import           Crypto.PubKey.ECC.Generate (generate)
import           Crypto.PubKey.ECC.ECDSA (Signature(..))
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECC
import           Crypto.PubKey.ECC.Types (CurveName(SEC_p256k1), getCurveByName)
import           Crypto.Random.Types (MonadRandom)

import           Codec.Serialise
import           Codec.Serialise.JSON
import qualified Data.ByteString.Lazy as LBS
import           Data.Text.Prettyprint.Doc
import           Data.Aeson (FromJSON(..), ToJSON(..), withObject, object, (.=), (.:))
import qualified Data.ByteString.Base64.Extended as Base64
import           Web.HttpApiData

data PublicKey = PublicKey ECDSA.PublicKey (Hashed ECDSA.PublicKey)
    deriving (Show, Generic)

mkPublicKey :: ECDSA.PublicKey -> PublicKey
mkPublicKey pk = PublicKey pk (hash pk)

instance Serialise PublicKey where
    encode (PublicKey pk _) = encode pk
    decode = mkPublicKey <$> decode

instance ToJSON PublicKey where
    toJSON = serialiseToJSON

instance FromJSON PublicKey where
    parseJSON = deserialiseParseJSON

instance Hashable PublicKey where
    hash (PublicKey _ h) = toHashed (fromHashed h)

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

publicKeyHash :: PublicKey -> Hashed ECDSA.PublicKey
publicKeyHash (PublicKey _ h) = h

newtype PrivateKey = PrivateKey ECDSA.PrivateKey
    deriving (Show, Eq)

--------------------------------------------------------------------------------

instance Ord Signature where
    (<=) a b = (sign_r a, sign_s a) <= (sign_r b, sign_s b)

instance ToJSON Signature where
    toJSON = serialiseToJSON

instance FromJSON Signature where
    parseJSON = deserialiseParseJSON

instance FromHttpApiData Signature where
    parseQueryParam _txt = notImplemented

deriving instance Generic Signature
instance Serialise Signature

-- | A signed message.
-- Create these with "sign" and verify them with "verify".
data Signed msg = Signed { sigMessage :: msg, sigSignature :: Signature }
    deriving (Show, Eq, Ord, Functor, Generic)

instance Serialise msg => Serialise (Signed msg)

instance Hashable msg => Hashable (Signed msg) where
    hash :: Signed msg -> Hashed (Signed msg)
    hash (Signed msg _) = toHashed (fromHashed (hash msg))

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


-- | Generate a new random keypair.
generateKeyPair :: MonadRandom m => m (PublicKey, PrivateKey)
generateKeyPair = do
    (pk, sk) <- generate (getCurveByName SEC_p256k1)
    pure (PublicKey pk (hash pk), PrivateKey sk)

-- | Sign a message with a private key.
sign :: (MonadRandom m, Serialise msg) => PrivateKey -> msg -> m (Signed msg)
sign (PrivateKey key) msg =
    Signed msg <$> ECDSA.sign key hashAlgorithm (LBS.toStrict $ serialise msg)

-- | Create a signed message from a message and a signature.
signed :: Signature -> msg -> Signed msg
signed sig msg = Signed msg sig

-- | Unwrap a signed message from its signature.
unsign :: Signed msg -> msg
unsign = sigMessage

-- | Verify a signed message with the public key.
verify :: Serialise msg => PublicKey -> Signed msg -> Bool
verify (PublicKey key _) (Signed msg sig) =
    ECDSA.verify hashAlgorithm key sig (LBS.toStrict $ serialise msg)
