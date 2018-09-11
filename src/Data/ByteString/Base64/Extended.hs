module Data.ByteString.Base64.Extended
    ( Base64 ()
    , encode
    , encodeLazy
    , encodeBinary
    , decode
    , decodeOrFail
    , decodeLazy
    , fromText
    ) where

import           Prelude

import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson hiding (encode, decode)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Text (Text)
import qualified Data.Binary as Binary
import           Data.Binary (Binary)

newtype Base64 a = Base64 ByteString
    deriving (Show, Eq, Ord)

instance ToJSON (Base64 a) where
    toJSON (Base64 bs) = String $ decodeUtf8 bs

instance FromJSON (Base64 a) where
    parseJSON = withText "Base64" $ pure . fromText

encode :: ByteString -> Base64 a
encode bs = Base64 (Base64.encode bs)

encodeLazy :: LBS.ByteString -> Base64 a
encodeLazy bs = Base64 (Base64.encode (LBS.toStrict bs))

encodeBinary :: Binary a => a -> Base64 a
encodeBinary x = encodeLazy (Binary.encode x)

decode :: Base64 a -> ByteString
decode (Base64 bs) = Base64.decodeLenient bs

decodeOrFail :: Base64 a -> Either String ByteString
decodeOrFail (Base64 bs) = Base64.decode bs

decodeLazy :: Base64 a -> LBS.ByteString
decodeLazy (Base64 bs) = LBS.fromStrict $ Base64.decodeLenient bs

fromText :: Text -> Base64 a
fromText = Base64 . encodeUtf8
