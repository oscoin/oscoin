module Oscoin.Crypto.Hash
    ( Hashed
    , Hashable(..)
    , hashed
    , fromHashed
    , hashAlgorithm
    , maxHash
    , zeroHash
    , toHex
    ) where

import           Oscoin.Prelude

import           Crypto.Hash (HashAlgorithm, Digest, Blake2b_256(..))
import qualified Crypto.Hash as Crypto
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import           Data.Binary (Binary)
import           Data.ByteArray (convert, zero)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base16 as Base16
import           Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (ToJSON(..), Value(String))
import           Web.HttpApiData (FromHttpApiData(..))

type Hashed a = Hashed' Blake2b_256 a

newtype Hashed' algo a = Hashed' { fromHashed :: Digest algo }
    deriving (Eq, Ord, Show, ByteArrayAccess)

instance ToJSON (Hashed' Blake2b_256 a) where
    toJSON (Hashed' digest) = toJSON digest

instance Binary (Hashed' Blake2b_256 a) where
    put = Binary.put <$> fromHashed
    get = Hashed' <$> Binary.get

instance FromHttpApiData (Hashed' Blake2b_256 a) where
    parseQueryParam txt =
        case fromHex (encodeUtf8 txt) of
            Left err -> Left (fromError err)
            Right bs -> Right $ Binary.decode $ LBS.fromStrict bs

hashed :: Crypto.Digest algo -> Hashed' algo a
hashed = Hashed'

hashAlgorithm :: Blake2b_256
hashAlgorithm = Blake2b_256

instance Binary (Digest Blake2b_256) where
    put digest =
        Binary.putByteString (convert digest :: ByteString)
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

-- TODO: Make result type polymorphic: `Either Error ba`.
fromHex :: ByteString -> Either Error ByteString
fromHex bs =
    case Base16.decode bs of
        (valid, "")  -> Right valid
        (_, invalid) -> Left $ Error ("Can't parse " <> tshow invalid)

-------------------------------------------------------------------------------

class Binary a => Hashable a where
    hash :: a -> Hashed a
    hash a =
        Hashed' . Crypto.hash . LBS.toStrict $ Binary.encode a

instance Hashable Text
instance Hashable ByteString
