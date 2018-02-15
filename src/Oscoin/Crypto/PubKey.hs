module Oscoin.Crypto.PubKey
    ( Signed(..)
    , generateKeyPair
    , sign
    , signed
    , unsign
    , verify
    , module Crypto.PubKey.ECC.ECDSA
    ) where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash (Hashed, hashAlgorithm, Hashable(..), hashed, fromHashed)

import           Crypto.PubKey.ECC.Generate (generate)
import           Crypto.PubKey.ECC.ECDSA (PublicKey(..), PrivateKey, Signature(..))
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Crypto.PubKey.ECC.Types (CurveName(SEC_p256k1), Point(..), getCurveByName)
import           Crypto.Random.Types (MonadRandom)

import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (FromJSON(..), ToJSON(..), withText, withObject, object, (.=), (.:))
import qualified Data.ByteString.Base64.Extended as Base64
import           Data.ByteString.Base64.Extended (Base64(..))
import           Web.HttpApiData

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

-- | A signed message.
-- Create these with "sign" and verify them with "verify".
data Signed msg = Signed { sigMessage :: msg, sigSignature :: Signature }
    deriving (Show, Eq, Ord, Functor, Generic)

type instance Id (Signed msg) = Hashed (Signed msg)

instance Binary msg => Binary (Signed msg)

instance Hashable msg => Hashable (Signed msg) where
    hash (Signed msg _) = hashed (fromHashed (hash msg))

instance ToJSON tx => ToJSON (Signed tx) where
    toJSON (Signed tx sig) =
        object [ "tx"  .= toJSON tx
               , "sig" .= toJSON sig
               ]

instance FromJSON tx => FromJSON (Signed tx) where
    parseJSON = withObject "Signed Tx" $ \o -> do
        tx  <- o .: "tx"
        sig <- o .: "sig"
        pure $ signed sig tx

instance Binary PublicKey where
    put (PublicKey curve (Point x y)) | curve == getCurveByName SEC_p256k1 =
        Binary.put x >> Binary.put y
    put _ =
        error "can't encode PublicKey curve or point type"
    get = do
        x <- Binary.get @Integer
        y <- Binary.get @Integer

        pure $ PublicKey (getCurveByName SEC_p256k1) (Point x y)

-- | Generate a new random keypair.
generateKeyPair :: MonadRandom m => m (PublicKey, PrivateKey)
generateKeyPair = do
    (pk, sk) <- generate (getCurveByName SEC_p256k1)
    pure (pk, sk)

-- | Sign a message with a private key.
sign :: (MonadRandom m, Binary msg) => PrivateKey -> msg -> m (Signed msg)
sign key msg = do
    sig <- ECDSA.sign key hashAlgorithm (LBS.toStrict $ Binary.encode msg)
    pure $ Signed msg sig

-- | Create a signed message from a message and a signature.
signed :: Signature -> msg -> Signed msg
signed sig msg = Signed msg sig

-- | Unwrap a signed message from its signature.
unsign :: Signed msg -> msg
unsign = sigMessage

-- | Verify a signed message with the public key.
verify :: Binary msg => PublicKey -> Signed msg -> Bool
verify key (Signed msg sig) =
    ECDSA.verify hashAlgorithm key sig (LBS.toStrict $ Binary.encode msg)
