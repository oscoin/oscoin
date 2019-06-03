{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Crypto.PubKey.Mock where

import           Oscoin.Prelude hiding (length)
import qualified Oscoin.Prelude as Prelude

import           Oscoin.Crypto (MockCrypto)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.Hash.Mock ()
import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.PubKey.Internal (PK(..), SK(..))

import           Codec.Serialise
import           Codec.Serialise.JSON (deserialiseParseJSON, serialiseToJSON)
import           Crypto.Random
import           Data.Aeson hiding (decode, encode)
import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H

newtype MockKey = MockKey [Word8] deriving (Eq, Ord, Show)

instance Crypto.Hashable MockCrypto MockKey where
    hash (MockKey w8s) = Crypto.toHashed . Crypto.fromHashed . Crypto.hash . BS.pack $ w8s

instance HasDigitalSignature MockCrypto where

    newtype PublicKey MockCrypto =
        MockPK (PK MockCrypto MockKey) deriving (Eq, Ord)
    newtype PrivateKey MockCrypto = MockSK (SK (MockKey, [Word8]))
        deriving (Eq)

    newtype Signature MockCrypto =
        MockSignature [Word8] deriving (Eq, Show, Ord)

    sign (MockSK (SK (MockKey pkSkXored, sk))) bytes = do
        let !sig = zipWith xor pkSkXored sk
        sig `deepseq` pure $! Signed bytes (MockSignature sig)

    verify (MockPK (PK (MockKey pk) _)) (Signed _ (MockSignature sig)) =
        sig == pk

    generateKeyPair = do
        pk  <- BS.unpack <$> getRandomBytes 8
        sk0 <- BS.unpack <$> getRandomBytes 8
        -- Pad the PrivateKey at the end, otherwise the xor trick won't work.
        let sk = if Prelude.length pk > Prelude.length sk0
                    then sk0 <> replicate (Prelude.length pk - Prelude.length sk0) 0
                    else sk0
        let !mockPk = MockPK $ PK (MockKey pk) (Crypto.hash (MockKey pk))
        let !mockSk = MockSK $ SK (MockKey $ zipWith xor pk sk, sk)
        pure ( mockPk, mockSk )

deriving instance Show (Crypto.Hash MockCrypto) => Show (PublicKey MockCrypto)

instance H.Hashable (PublicKey MockCrypto) where
    hashWithSalt salt (MockPK (PK _ h)) = H.hashWithSalt salt . Crypto.fromHashed $ h

instance Serialise (PublicKey MockCrypto) where
    encode (MockPK (PK (MockKey pk) _)) = encode pk
    decode = (\pk -> MockPK $ PK @MockCrypto (MockKey pk) (Crypto.hash (MockKey pk))) <$> decode

instance ToJSON (PublicKey MockCrypto) where
    toJSON = serialiseToJSON

instance FromJSON (PublicKey MockCrypto) where
    parseJSON = deserialiseParseJSON

instance ByteArrayAccess (PublicKey MockCrypto) where
    length             = fromIntegral . LBS.length . serialise
    withByteArray pk f = withByteArray (LBS.toStrict (serialise pk)) f

instance Eq a => Eq (PK MockCrypto a) where
    (PK a1 b1) == (PK a2 b2) = a1 == a2 && b1 == b2

instance Ord a => Ord (PK MockCrypto a) where
    compare (PK a1 b1) (PK a2 b2) =
        mconcat [a1 `compare` a2, b1 `compare` b2]

instance Serialise (Signature MockCrypto) where
    encode (MockSignature bs) = encode bs
    decode = MockSignature <$> decode

instance Crypto.Hashable MockCrypto (PublicKey MockCrypto) where
    hash (MockPK (PK _ h)) = Crypto.toHashed $ Crypto.fromHashed h

instance ToJSON (Signature MockCrypto) where
    toJSON = serialiseToJSON

instance FromJSON (Signature MockCrypto) where
    parseJSON = deserialiseParseJSON
