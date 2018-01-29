module Oscoin.Tests where

import Oscoin.Prelude
import Oscoin.Environment
import Oscoin.HTTP (mkMiddleware)

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

import qualified Network.Wai.Test as Wai
import qualified Network.Wai      as Wai
import           Web.Spock (spockAsApp)
import           Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Header as HTTP

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase "API" testOscoinAPI ]

runSession :: Wai.Session () -> Assertion
runSession sess =
    spockAsApp (mkMiddleware Testing) >>= Wai.runSession sess

testOscoinAPI :: Assertion
testOscoinAPI = runSession $ do
    resp <- request GET "/" [] noBody
    Wai.assertStatus 200 resp

request
    :: Aeson.ToJSON a
    => HTTP.StdMethod
    -> ByteString
    -> HTTP.RequestHeaders
    -> Maybe a
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

-- | Represents an empty request body.
noBody :: Maybe ()
noBody = Nothing
