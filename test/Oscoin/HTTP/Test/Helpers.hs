module Oscoin.HTTP.Test.Helpers where

import Oscoin.Prelude
import Oscoin.Environment
import Oscoin.HTTP (mkMiddleware)

import Test.Tasty.HUnit (Assertion)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

import qualified Network.Wai.Test as Wai
import qualified Network.Wai      as Wai
import           Web.Spock (spockAsApp)
import           Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Header as HTTP

-- | Like "Assertion" but bound to a user session (cookies etc.)
type Session = Wai.Session

-- | Turn a "Session" into an "Assertion".
runSession :: Session () -> Assertion
runSession sess =
    spockAsApp (mkMiddleware Testing) >>= Wai.runSession sess

-- TODO: Should also assert status is 2xx.
assertBody :: HasCallStack => Aeson.ToJSON a => a -> Wai.SResponse -> Wai.Session ()
assertBody obj = Wai.assertBody (Aeson.encode obj)

-- | Assert a response status is 200 OK.
assertOK :: HasCallStack => Wai.SResponse -> Wai.Session ()
assertOK = Wai.assertStatus 200

-- | Assert a response status is the given code.
assertStatus :: HasCallStack => Int -> Wai.SResponse -> Wai.Session ()
assertStatus = Wai.assertStatus

-- | Low-level HTTP request helper.
request
    :: Aeson.ToJSON a
    => HTTP.StdMethod                    -- ^ Request method
    -> ByteString                        -- ^ Request path
    -> HTTP.RequestHeaders               -- ^ Request headers
    -> Maybe a                           -- ^ Request body
    -> Wai.Session Wai.SResponse
request method path headers body =
    Wai.srequest $ Wai.SRequest (Wai.setPath req path) (reqBody body)
  where
    req = Wai.defaultRequest
        { Wai.requestMethod = HTTP.renderStdMethod method
        , Wai.requestHeaders = headers
        }
    reqBody Nothing    = LBS.empty
    reqBody (Just obj) = Aeson.encode obj

-- | A GET request.
get :: ByteString -> Wai.Session Wai.SResponse
get path = request GET path [] noBody

-- | A DELETE request.
delete :: ByteString -> Wai.Session Wai.SResponse
delete path = request DELETE path [] noBody

-- | A PUT request.
put :: Aeson.ToJSON a => ByteString -> a -> Wai.Session Wai.SResponse
put path body =
    request PUT path [] (Just body)

-- | A POST request.
post :: Aeson.ToJSON a => ByteString -> a -> Wai.Session Wai.SResponse
post path body =
    request POST path [] (Just body)

-- | Represents an empty request body.
noBody :: Maybe ()
noBody = Nothing

-- | Helper for string literals.
t :: Text -> Text
t = id
