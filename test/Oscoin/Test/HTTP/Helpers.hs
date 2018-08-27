{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: Move this module to Network.Wai.Test.Extended
module Oscoin.Test.HTTP.Helpers where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock, Block)
import           Oscoin.Environment
import           Oscoin.HTTP.API (api)
import           Oscoin.HTTP.Internal (mkMiddleware)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.Storage.Block as BlockStore

import           Oscoin.Test.Consensus.Node (DummyNodeId)

import           Test.Tasty.HUnit (Assertion, assertFailure)
import qualified Test.Tasty.HUnit as Tasty

import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Network.HTTP.Types.Header as HTTP
import           Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import           Web.Spock (spockAsApp)

-- | Like "Assertion" but bound to a user session (cookies etc.)
type Session = Wai.Session

-- | Dummy transaction type used for testing.
type DummyTx = ()

instance Semigroup a => Semigroup (Session a) where
    (<>) = liftA2 (<>)

instance (Monoid a, Semigroup a) => Monoid (Session a) where
    mempty = pure mempty
    mappend = (<>)

instance MonadRandom Session where
    getRandomBytes = io . getRandomBytes

-- | Turn a "Session" into an "Assertion".
runSession :: Node.Config -> DummyNodeId -> Session () -> Assertion
runSession cfg nid sess = do
    mp <- Mempool.newIO
    st <- STree.connect
    bs <- BlockStore.newIO $ genesisBlockStore (emptyGenesisBlock 0 :: Block DummyTx ())
    nh <- Node.open cfg nid mp st bs

    app <- spockAsApp (mkMiddleware (api Testing) nh)
    Wai.runSession sess app

infix 1 @?=, @=?

(@?=) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(@?=) actual expected = io $ Tasty.assertEqual "" expected actual

(@=?) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
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
        , Wai.requestHeaders = (HTTP.hAccept, "application/json") : headers
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
    request PUT path [(HTTP.hContentType, "application/json")] (Just body)

-- | A POST request.
post :: Aeson.ToJSON a => Text -> a -> Wai.Session Wai.SResponse
post path body =
    request POST path [(HTTP.hContentType, "application/json")] (Just body)

-- | Represents an empty request body.
noBody :: Maybe ()
noBody = Nothing

-- | Helper for string literals.
t :: Text -> Text
t = identity

responseBody :: Wai.SResponse -> LBS.ByteString
responseBody = Wai.simpleBody

jsonBody :: HasCallStack => Wai.SResponse -> Wai.Session Aeson.Value
jsonBody resp =
    let body = Wai.simpleBody resp
        in case Aeson.decode body of
            Just obj -> pure obj
            Nothing  -> io $ assertFailure
                ("Could not decode body as JSON:\n`" <> L8.unpack body <> "`\n")
