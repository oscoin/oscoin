module Oscoin.Crypto.Hash
    ( Hashed
    , hash
    , hashed
    , hashAlgorithm
    , maxHash
    , zeroHash
    , toHex
    ) where

import           Oscoin.Prelude

import           Crypto.Hash (HashAlgorithm, Digest, Blake2b_256(..))
import qualified Crypto.Hash as Crypto
import qualified Data.Binary as Binary
import           Data.ByteArray (convert, zero)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base16 as Base16
import           Data.Maybe (fromJust)
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (ToJSON(..), Value(String))

type Hashed a = Hashed' Blake2b_256 a

newtype Hashed' algo a = Hashed' { fromHashed :: Digest algo }
    deriving (Eq, Ord, Show, ByteArrayAccess)

instance ToJSON (Hashed' Blake2b_256 a) where
    toJSON (Hashed' digest) = toJSON digest

instance Binary (Hashed' Blake2b_256 a) where
    put = Binary.put <$> fromHashed
    get = Hashed' <$> Binary.get

hash :: Binary a => a -> Hashed a
hash a =
    Hashed' . Crypto.hash . LBS.toStrict $ Binary.encode a

hashed :: Crypto.Digest algo -> Hashed' algo a
hashed = Hashed'

hashAlgorithm :: Blake2b_256
hashAlgorithm = Blake2b_256

instance Binary (Digest Blake2b_256) where
    put digest =
        Binary.put (convert digest :: ByteString)
    get =
        fromJust . Crypto.digestFromByteString <$> Binary.get @ByteString

instance ToJSON (Digest Blake2b_256) where
    toJSON digest =
        String $ decodeUtf8 $ toHex $ LBS.toStrict $ Binary.encode digest

maxHash :: HashAlgorithm a => Digest a
maxHash = fromJust $
    Crypto.digestFromByteString (ByteArray.replicate (Crypto.hashDigestSize Blake2b_256) maxBound :: ByteString)

zeroHash :: HashAlgorithm a => Digest a
zeroHash = fromJust $
    Crypto.digestFromByteString (zero (Crypto.hashDigestSize Blake2b_256) :: ByteString)

toHex :: ByteArrayAccess ba => ba -> ByteString
toHex bs =
    Base16.encode $ convert bs

