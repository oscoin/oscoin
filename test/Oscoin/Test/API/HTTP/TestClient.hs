-- | Provide a 'Client' object for a WAI Test 'Session'.
--
-- Re-exports "Oscoin.API.Client".
--
-- @
-- import qualified Oscoin.Test.API.HTTP.TestClient as Client
--
-- flip runSession app $ do
--     result <- Client.submitTransaction Client.session tx
-- @
--
module Oscoin.Test.API.HTTP.TestClient
    ( module Oscoin.API.Client
    , session
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Client
import           Oscoin.API.Client.HTTP
import           Oscoin.Data.Tx

import           Oscoin.Test.Crypto
import           Oscoin.Test.HTTP.Helpers (DummySeal, Session, liftWaiSession)

import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai


session :: (IsCrypto c) => Client c (Session c (Tx c) DummySeal)
session = httpClientFromRequest makeWaiRequest

makeWaiRequest :: Request -> Session c (Tx c) DummySeal Response
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
