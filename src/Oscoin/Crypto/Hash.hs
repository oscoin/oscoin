module Oscoin.Crypto.Hash
    ( Hashed
    , hash
    , maxHash
    ) where

import           Oscoin.Prelude

import           Crypto.Hash (HashAlgorithm, Digest, Blake2b_256(..))
import qualified Crypto.Hash as Crypto
import qualified Data.Binary as Binary
import           Data.ByteArray (convert)
import qualified Data.ByteArray as ByteArray
import           Data.Maybe (fromJust)
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as LBS

type Hashed a = Hashed' Crypto.Blake2b_256 a

newtype Hashed' algo a = Hashed' (Crypto.Digest algo)
    deriving (Eq, Ord)

hash :: Binary a => a -> Hashed a
hash a =
    Hashed' . Crypto.hash . LBS.toStrict $ Binary.encode a

instance Binary (Digest Blake2b_256) where
    put digest =
        Binary.put (convert digest :: ByteString)
    get =
        fromJust . Crypto.digestFromByteString <$> Binary.get @ByteString

maxHash :: HashAlgorithm a => Digest a
maxHash = fromJust $
    Crypto.digestFromByteString (ByteArray.replicate (Crypto.hashDigestSize Blake2b_256) maxBound :: ByteString)


