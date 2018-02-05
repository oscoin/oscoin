module Oscoin.Crypto.PubKey
    ( Signed(..)
    , generateKeyPair
    , sign
    , verify
    , module Crypto.PubKey.ECC.ECDSA
    ) where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash (hashAlgorithm)

import           Crypto.PubKey.ECC.Generate (generate)
import           Crypto.PubKey.ECC.ECDSA (PublicKey(..), PrivateKey, Signature)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Crypto.PubKey.ECC.Types (CurveName(SEC_p256k1), Point(..), getCurveByName)
import           Crypto.Random.Types (MonadRandom)

import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS

-- | A signed message.
-- Create these with "sign" and verify them with "verify".
data Signed msg = Signed { sigMessage :: msg, sigSignature :: Signature }
    deriving (Functor, Eq)

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

-- | Verify a signed message with the public key.
verify :: Binary msg => PublicKey -> Signed msg -> Bool
verify key (Signed msg sig) =
    ECDSA.verify hashAlgorithm key sig (LBS.toStrict $ Binary.encode msg)
