{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Crypto.PubKey.Mock where

import           Oscoin.Prelude

import           Oscoin.Crypto (MockCrypto)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.Hash.Mock ()
import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.PubKey.Internal (PrivateKey(..), PublicKey(..))

import           Codec.Serialise
import           Codec.Serialise.JSON (deserialiseParseJSON, serialiseToJSON)
import           Crypto.Random
import           Data.Aeson hiding (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.Hashable as H

newtype MockKey = MockKey [Word8] deriving (Eq, Show)

instance Crypto.Hashable MockCrypto MockKey where
    hash (MockKey w8s) = Crypto.toHashed . Crypto.fromHashed . Crypto.hash . BS.pack $ w8s

instance HasDigitalSignature MockCrypto where

    newtype PK MockCrypto =
        MockPK (PublicKey MockCrypto MockKey) deriving Eq
    newtype SK MockCrypto = MockSK (PrivateKey (MockKey, [Word8]))

    newtype Signature MockCrypto =
        MockSignature [Word8] deriving (Eq, Show)

    sign (MockSK (PrivateKey (MockKey pkSkXored, sk))) bytes = do
        let !sig = zipWith xor pkSkXored sk
        sig `deepseq` pure $! Signed bytes (MockSignature sig)

    verify (MockPK (PublicKey (MockKey pk) _)) (Signed _ (MockSignature sig)) =
        sig == pk

    generateKeyPair = do
        pk  <- BS.unpack <$> getRandomBytes 8
        sk0 <- BS.unpack <$> getRandomBytes 8
        -- Pad the SK at the end, otherwise the xor trick won't work.
        let sk = if length pk > length sk0
                    then sk0 <> replicate (length pk - length sk0) 0
                    else sk0
        let !mockPk = MockPK $ PublicKey (MockKey pk) (Crypto.hash (MockKey pk))
        let !mockSk = MockSK $ PrivateKey (MockKey $ zipWith xor pk sk, sk)
        pure ( mockPk, mockSk )

deriving instance Show (Crypto.Hash MockCrypto) => Show (PK MockCrypto)

instance H.Hashable (PK MockCrypto) where
    hashWithSalt salt (MockPK (PublicKey _ h)) = H.hashWithSalt salt . Crypto.fromHashed $ h

instance Serialise (PK MockCrypto) where
    encode (MockPK (PublicKey (MockKey pk) _)) = encode pk
    decode = (\pk -> MockPK $ PublicKey @MockCrypto (MockKey pk) (Crypto.hash (MockKey pk))) <$> decode

instance ToJSON (PK MockCrypto) where
    toJSON = serialiseToJSON

instance FromJSON (PK MockCrypto) where
    parseJSON = deserialiseParseJSON

instance Eq a => Eq (PublicKey MockCrypto a) where
    (PublicKey a1 b1) == (PublicKey a2 b2) = a1 == a2 && b1 == b2

instance Serialise (Signature MockCrypto) where
    encode (MockSignature bs) = encode bs
    decode = MockSignature <$> decode

instance Crypto.Hashable MockCrypto (PK MockCrypto) where
    hash (MockPK (PublicKey _ h)) = Crypto.toHashed $ Crypto.fromHashed h

instance ToJSON (Signature MockCrypto) where
    toJSON = serialiseToJSON

instance FromJSON (Signature MockCrypto) where
    parseJSON = deserialiseParseJSON
