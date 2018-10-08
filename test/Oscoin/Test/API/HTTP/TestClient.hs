-- | Provide an instance of 'MonadClient' that can be run in a WAI Test
-- 'Session'.
--
-- @
-- import qualified Oscoin.Test.API.HTTP.TestClient as Client
--
-- flip runSession app $ do
--     result <- Client.run $ Client.submitTransaction tx
-- @
--
module Oscoin.Test.API.HTTP.TestClient
    ( module Oscoin.API.Client
    , TestClient
    , run
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Client
import           Oscoin.API.Types
import           Oscoin.Crypto.Hash (fromHashed)

import           Codec.Serialise
import qualified Data.Text as T
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Test
import           Web.HttpApiData (toUrlPiece)



newtype TestClient a = TestClient { run :: Session a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadClient TestClient where
    submitTransaction tx =
        post "/transactions" tx

    getTransaction txId =
        get $ "/transactions/" <> toUrlPiece (fromHashed txId)

    getState key =
        get $ "/state?q=[" <> T.intercalate "," key <> "]"


get :: (Serialise a) => Text -> TestClient (Result a)
get reqPath = TestClient $ deserialiseResponse <$> request req
  where
    req = flip setPath (encodeUtf8 reqPath) $ defaultRequest
        { requestMethod = methodGet
        , rawPathInfo = encodeUtf8 reqPath
        , requestHeaders = [(hAccept, "application/cbor")]
        }

post :: (Serialise a, Serialise b) => Text -> a -> TestClient (Result b)
post reqPath reqBody = TestClient $ deserialiseResponse <$> srequest sreq
  where
    sreq = SRequest req (serialise reqBody)
    req = flip setPath (encodeUtf8 reqPath) $ defaultRequest
        { requestMethod = methodPost
        , rawPathInfo = encodeUtf8 reqPath
        , requestHeaders = [(hAccept, "application/cbor"), (hContentType, "application/cbor")]
        }

deserialiseResponse :: (Serialise a) => SResponse -> Result a
deserialiseResponse response =
    case deserialiseOrFail $ simpleBody response of
        Left _    -> Err $ "Failed to deserialise response: " <> show (simpleBody response)
        Right val -> val
