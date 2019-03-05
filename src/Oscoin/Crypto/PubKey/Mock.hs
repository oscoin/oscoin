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
import qualified Codec.Serialise.Encoding as CBOR
import           Codec.Serialise.JSON (deserialiseParseJSON, serialiseToJSON)
import           Crypto.Random
import           Data.Aeson hiding (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.Hashable as H

instance HasDigitalSignature MockCrypto where

    newtype PK MockCrypto =
        MockPK (PublicKey MockCrypto ByteString) deriving Eq
    newtype SK MockCrypto = MockSK (PrivateKey (ByteString, [Word8]))

    newtype Signature MockCrypto =
        MockSignature ByteString deriving (Eq, Show)

    sign (MockSK (PrivateKey (pkSkXored, sk))) bytes =
        let sig = BS.pack . BS.zipWith xor pkSkXored . BS.pack $ sk
        in pure $ Signed bytes (MockSignature sig)

    verify (MockPK (PublicKey pk _)) (Signed _ (MockSignature sig)) =
        sig == pk

    generateKeyPair = do
        pk  <- getRandomBytes 16
        sk0 <- BS.unpack <$> getRandomBytes 16
        -- Pad the SK at the end, otherwise the xor trick won't work.
        let sk = if BS.length pk > length sk0
                    then sk0 <> replicate (BS.length pk - length sk0) 0
                    else sk0
        pure ( MockPK $ PublicKey pk (Crypto.hash pk)
             , MockSK $ PrivateKey (BS.pack $ BS.zipWith xor pk (BS.pack sk), sk)
             )

deriving instance Show (Crypto.Hash MockCrypto) => Show (PK MockCrypto)

instance H.Hashable (PK MockCrypto) where
    hashWithSalt salt (MockPK (PublicKey _ h)) = H.hashWithSalt salt . Crypto.fromHashed $ h

instance Serialise (PK MockCrypto) where
    encode (MockPK (PublicKey pk _)) = encode pk
    decode = (\pk -> MockPK $ PublicKey @MockCrypto pk (Crypto.hash pk)) <$> decode

instance ToJSON (PK MockCrypto) where
    toJSON = serialiseToJSON

instance FromJSON (PK MockCrypto) where
    parseJSON = deserialiseParseJSON

instance Eq (PublicKey MockCrypto ByteString) where
    (PublicKey a1 b1) == (PublicKey a2 b2) = a1 == a2 && b1 == b2

instance Serialise (Signature MockCrypto) where
    encode (MockSignature bs) = CBOR.encodeBytes bs
    decode = MockSignature <$> decode

instance Crypto.Hashable MockCrypto (PK MockCrypto) where
    hash (MockPK (PublicKey _ h)) = Crypto.toHashed $ Crypto.fromHashed h

instance ToJSON (Signature MockCrypto) where
    toJSON = serialiseToJSON

instance FromJSON (Signature MockCrypto) where
    parseJSON = deserialiseParseJSON
