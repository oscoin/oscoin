module Oscoin.Crypto.Hash
    ( Hashed
    , hash
    , hashed
    , maxHash
    , zeroHash
    ) where

import           Oscoin.Prelude

import           Crypto.Hash (HashAlgorithm, Digest, Blake2b_256(..))
import qualified Crypto.Hash as Crypto
import qualified Data.Binary as Binary
import           Data.ByteArray (convert, zero)
import qualified Data.ByteArray as ByteArray
import           Data.Maybe (fromJust)
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as LBS

type Hashed a = Hashed' Blake2b_256 a

newtype Hashed' algo a = Hashed' { fromHashed :: Crypto.Digest algo }
    deriving (Eq, Ord, Show)

instance Binary (Hashed' Blake2b_256 a) where
    put = Binary.put <$> fromHashed
    get = Hashed' <$> Binary.get

hash :: Binary a => a -> Hashed a
hash a =
    Hashed' . Crypto.hash . LBS.toStrict $ Binary.encode a

hashed :: Crypto.Digest algo -> Hashed' algo a
hashed = Hashed'

instance Binary (Digest Blake2b_256) where
    put digest =
        Binary.put (convert digest :: ByteString)
    get =
        fromJust . Crypto.digestFromByteString <$> Binary.get @ByteString

maxHash :: HashAlgorithm a => Digest a
maxHash = fromJust $
    Crypto.digestFromByteString (ByteArray.replicate (Crypto.hashDigestSize Blake2b_256) maxBound :: ByteString)

zeroHash :: HashAlgorithm a => Digest a
zeroHash = fromJust $
    Crypto.digestFromByteString (zero (Crypto.hashDigestSize Blake2b_256) :: ByteString)

