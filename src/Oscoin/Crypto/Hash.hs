module Oscoin.Crypto.Hash
    ( Hashed
    , Hashable(..)
    , hashed
    , fromHashed
    , HashAlgorithm
    , hashAlgorithm
    , maxHash
    , zeroHash
    , toHex
    , fromHex
    ) where

import           Oscoin.Prelude

import           Crypto.Hash (Digest, Blake2b_256(..))
import qualified Crypto.Hash as Crypto
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary
import           Data.Binary (Binary)
import           Data.ByteArray (convert, zero)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base16 as Base16
import           Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(String), withText)
import qualified Data.Text as T
import           Web.HttpApiData (FromHttpApiData(..))

type Hashed a = Hashed' Blake2b_256 a

newtype Hashed' algo a = Hashed' { fromHashed :: Digest algo }
    deriving (Eq, Ord, Show, ByteArrayAccess)

instance ToJSON (Hashed' Blake2b_256 a) where
    toJSON (Hashed' digest) = toJSON digest

instance FromJSON (Hashed' Blake2b_256 a) where
    parseJSON = withText "Hashed' Blake2b_256 a" $ \t ->
        case fromHex (encodeUtf8 t) of
            Right bs -> pure $ Binary.decode (LBS.fromStrict bs)
            Left err -> fail (T.unpack $ fromError err)

instance Binary (Hashed' Blake2b_256 a) where
    put = Binary.put <$> fromHashed
    get = hashed <$> Binary.get

instance FromHttpApiData (Hashed' Blake2b_256 a) where
    parseQueryParam txt =
        case fromHex (encodeUtf8 txt) of
            Left err -> Left (fromError err)
            Right bs -> Right $ Binary.decode $ LBS.fromStrict bs

hashed :: Crypto.Digest algo -> Hashed' algo a
hashed = Hashed'

type HashAlgorithm = Blake2b_256

hashAlgorithm :: Blake2b_256
hashAlgorithm = Blake2b_256

instance Binary (Digest Blake2b_256) where
    put digest =
        Binary.putByteString (convert digest :: ByteString)
    get =
        fromJust . Crypto.digestFromByteString <$> Binary.getByteString size
      where
        size = Crypto.hashDigestSize Blake2b_256

instance ToJSON (Digest Blake2b_256) where
    toJSON digest =
        String $ decodeUtf8 $ toHex $ LBS.toStrict $ Binary.encode digest

maxHash :: Crypto.HashAlgorithm a => Digest a
maxHash = fromJust $
    Crypto.digestFromByteString (ByteArray.replicate (Crypto.hashDigestSize Blake2b_256) maxBound :: ByteString)

zeroHash :: Crypto.HashAlgorithm a => Digest a
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
        hashed . Crypto.hash . LBS.toStrict $ Binary.encode a

instance Hashable Text
instance Hashable ByteString
