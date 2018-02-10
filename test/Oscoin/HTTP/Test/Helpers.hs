-- TODO: Move this module to Network.Wai.Test.Extended
module Oscoin.HTTP.Test.Helpers where

import Oscoin.Prelude
import Oscoin.Environment
import Oscoin.Org (Org, OrgId)
import Oscoin.HTTP.Internal (mkMiddleware)
import Oscoin.HTTP.API (api)

import Test.Tasty.HUnit (Assertion, assertFailure)
import qualified Test.Tasty.HUnit as Tasty

import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as LBS

import qualified Network.Wai.Test as Wai
import qualified Network.Wai      as Wai
import           Web.Spock (spockAsApp)
import           Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Header as HTTP

-- | Like "Assertion" but bound to a user session (cookies etc.)
type Session = Wai.Session

instance Monoid a => Monoid (Session a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance MonadRandom Session where
    getRandomBytes = io . getRandomBytes

-- | Turn a "Session" into an "Assertion".
runSession :: [(OrgId, Org)] -> Session () -> Assertion
runSession orgs sess =
    spockAsApp (mkMiddleware (api Testing) orgs) >>= Wai.runSession sess

infix 1 @?=, @=?

(@?=) :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
(@?=) a b = io $ Tasty.assertEqual "" a b

(@=?) :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
(@=?) = flip (@?=)

-- TODO: Should also assert status is 2xx.
assertBody :: HasCallStack => Aeson.ToJSON a => a -> Wai.SResponse -> Wai.Session ()
assertBody obj = Wai.assertBody (Aeson.encode obj)

assertJSON :: HasCallStack => Wai.SResponse -> Wai.Session ()
assertJSON = void . jsonBody

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
    -> Text                              -- ^ Request path
    -> HTTP.RequestHeaders               -- ^ Request headers
    -> Maybe a                           -- ^ Request body
    -> Wai.Session Wai.SResponse
request method (encodeUtf8 -> path) headers body =
    Wai.srequest $ Wai.SRequest (Wai.setPath req path) (reqBody body)
  where
    req = Wai.defaultRequest
        { Wai.requestMethod = HTTP.renderStdMethod method
        , Wai.requestHeaders = headers
        }
    reqBody Nothing    = LBS.empty
    reqBody (Just obj) = Aeson.encode obj

-- | A GET request.
get :: Text -> Wai.Session Wai.SResponse
get path = request GET path [] noBody

-- | A DELETE request.
delete :: Text -> Wai.Session Wai.SResponse
delete path = request DELETE path [] noBody

-- | A PUT request.
put :: Aeson.ToJSON a => Text -> a -> Wai.Session Wai.SResponse
put path body =
    request PUT path [] (Just body)

-- | A POST request.
post :: Aeson.ToJSON a => Text -> a -> Wai.Session Wai.SResponse
post path body =
    request POST path [] (Just body)

-- | Represents an empty request body.
noBody :: Maybe ()
noBody = Nothing

-- | Helper for string literals.
t :: Text -> Text
t = id

responseBody :: Wai.SResponse -> LBS.ByteString
responseBody = Wai.simpleBody

jsonBody :: HasCallStack => Wai.SResponse -> Wai.Session Aeson.Value
jsonBody resp =
    let body = Wai.simpleBody resp
        in case Aeson.decode body of
            Just obj -> pure obj
            Nothing  -> io $ assertFailure
                ("Could not decode body as JSON:\n`" <> (L8.unpack body) <> "`\n")
