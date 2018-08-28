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
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (FromJSON(..), ToJSON(..), withText, withObject, object, (.=), (.:))
import qualified Data.ByteString.Base64.Extended as Base64
import           Data.ByteString.Base64.Extended (Base64(..))
import           Web.HttpApiData

data PublicKey = PublicKey ECDSA.PublicKey (Hashed ECDSA.PublicKey)
    deriving (Show, Generic)

instance Serialise PublicKey

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
    toJSON = toJSON . Base64.encodeLazy . Binary.encode

instance FromJSON Signature where
    parseJSON = withText "Signature" $
        -- TODO: Can we use the FromJSON instance of Base64?
        pure . Binary.decode . Base64.decodeLazy . Base64 . encodeUtf8

instance Binary Signature where
    put sig =
        Binary.put (sign_r sig, sign_s sig)
    get = do
        (sign_r, sign_s) <- Binary.get
        pure Signature{..}

instance FromHttpApiData Signature where
    parseQueryParam _txt = notImplemented

deriving instance Generic Signature
instance Serialise Signature

-- | A signed message.
-- Create these with "sign" and verify them with "verify".
data Signed msg = Signed { sigMessage :: msg, sigSignature :: Signature }
    deriving (Show, Eq, Ord, Functor, Generic)

instance Binary msg => Binary (Signed msg)
instance Serialise msg => Serialise (Signed msg)

instance Hashable msg => Hashable (Signed msg) where
    hash :: Signed msg -> Hashed (Signed msg)
    hash (Signed msg _) = toHashed (fromHashed (hash msg))

instance ToJSON msg => ToJSON (Signed msg) where
    toJSON (Signed msg sig) =
        object [ "msg" .= toJSON msg
               , "sig" .= toJSON sig
               ]

instance FromJSON msg => FromJSON (Signed msg) where
    parseJSON = withObject "Signed Tx" $ \o -> do
        msg <- o .: "msg"
        sig <- o .: "sig"
        pure $ signed sig msg

instance Binary PublicKey where
    put (PublicKey key _) = Binary.put key
    get = do
        key <- Binary.get
        pure $ PublicKey key (hash key)

-- | Generate a new random keypair.
generateKeyPair :: MonadRandom m => m (PublicKey, PrivateKey)
generateKeyPair = do
    (pk, sk) <- generate (getCurveByName SEC_p256k1)
    pure (PublicKey pk (hash pk), PrivateKey sk)

-- | Sign a message with a private key.
sign :: (MonadRandom m, Binary msg) => PrivateKey -> msg -> m (Signed msg)
sign (PrivateKey key) msg =
    Signed msg <$> ECDSA.sign key hashAlgorithm (LBS.toStrict $ Binary.encode msg)

-- | Create a signed message from a message and a signature.
signed :: Signature -> msg -> Signed msg
signed sig msg = Signed msg sig

-- | Unwrap a signed message from its signature.
unsign :: Signed msg -> msg
unsign = sigMessage

-- | Verify a signed message with the public key.
verify :: Binary msg => PublicKey -> Signed msg -> Bool
verify (PublicKey key _) (Signed msg sig) =
    ECDSA.verify hashAlgorithm key sig (LBS.toStrict $ Binary.encode msg)
