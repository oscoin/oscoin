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
import           Oscoin.API.HTTP.Client

import           Oscoin.Test.HTTP.Helpers (Session, liftWaiSession)

import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai


type TestClient = HttpClientT Session

run :: TestClient a -> Session a
run = runHttpClientT' makeWaiRequest

makeWaiRequest :: Request -> Session Response
makeWaiRequest Request{..} = liftWaiSession $ fromSresp <$> Wai.srequest sreq
  where
    sreq = Wai.SRequest req requestBody
    req = Wai.defaultRequest
        { Wai.requestMethod = requestMethod
        , Wai.requestHeaders = requestHeaders
        }
        & flip Wai.setPath (encodeUtf8 requestPath)
    fromSresp Wai.SResponse {..} =
        Response
            { responseStatus = simpleStatus
            , responseBody = simpleBody
            }
