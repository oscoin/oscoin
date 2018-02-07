module Data.ByteString.Base64.Extended where

import           Prelude

import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

newtype Base64 a = Base64 ByteString
    deriving (Show, Eq, Ord)

instance ToJSON (Base64 a) where
    toJSON (Base64 bs) = String $ decodeUtf8 bs

instance FromJSON (Base64 a) where
    parseJSON = withText "Base64" $ \t ->
        pure $ Base64 (encodeUtf8 t)

encode :: ByteString -> Base64 a
encode bs = Base64 (Base64.encode bs)

encodeLazy :: LBS.ByteString -> Base64 a
encodeLazy bs = Base64 (Base64.encode (LBS.toStrict bs))

decode :: Base64 a -> ByteString
decode (Base64 bs) = Base64.decodeLenient bs
