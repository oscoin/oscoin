{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: Move this module to Network.Wai.Test.Extended
module Oscoin.Test.HTTP.Helpers where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock)
import           Oscoin.Environment
import           Oscoin.HTTP.API (api)
import           Oscoin.HTTP.Internal (mkMiddleware, decode, encode, parseContentType, ContentType(..))
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Data.Tx (Tx)

import           Oscoin.Test.Consensus.Node (DummyNodeId)

import           Test.Tasty.HUnit (Assertion)
import qualified Test.Tasty.HUnit as Tasty

import           Crypto.Random.Types (MonadRandom(..))
import           Data.List (lookup)
import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON, FromJSON)
import           Codec.Serialise (Serialise)
import qualified Data.ByteString.Lazy as LBS

import qualified Network.HTTP.Types.Header as HTTP
import           Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import           Web.Spock (spockAsApp)

import qualified Radicle as Rad

-- | Like "Assertion" but bound to a user session (cookies etc.)
type Session = Wai.Session

-- | Dummy transaction type used for testing.
type DummyTx = Tx Rad.Value

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
    st <- STree.new
    bs <- BlockStore.newIO $ genesisBlockStore $ emptyGenesisBlock @DummyTx 0
    nh <- Node.open cfg nid mp st bs

    app <- spockAsApp (mkMiddleware (api Testing) nh)
    Wai.runSession sess app

infix 1 @?=, @=?

(@?=) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(@?=) actual expected = io $ Tasty.assertEqual "" expected actual

(@=?) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(@=?) = flip (@?=)

assertBody :: (HasCallStack, FromJSON a, Serialise a, Eq a, Show a) => a -> Wai.SResponse -> Wai.Session ()
assertBody expected resp = io $ case decodeBody resp of
    Left err -> Tasty.assertFailure $ show err
    Right actual -> Tasty.assertEqual "" expected actual

-- | Assert a response status is the given code.
assertStatus :: HasCallStack => Int -> Wai.SResponse -> Wai.Session ()
assertStatus = Wai.assertStatus

-- | Low-level HTTP request helper.
request
    :: HTTP.StdMethod                    -- ^ Request method
    -> Text                              -- ^ Request path
    -> HTTP.RequestHeaders               -- ^ Request headers
    -> LBS.ByteString                    -- ^ Request body
    -> Wai.Session Wai.SResponse
request method (encodeUtf8 -> path) headers body =
    Wai.srequest $ Wai.SRequest (mkRequest method path headers) body

mkRequest :: HTTP.StdMethod -> ByteString -> HTTP.RequestHeaders -> Wai.Request
mkRequest method path headers = Wai.setPath req path where
    req = Wai.defaultRequest
        { Wai.requestMethod = HTTP.renderStdMethod method
        , Wai.requestHeaders = headers
        }

-- | @Codec@ represents the @ContentType@ of sent HTTP request bodies and
-- the accepted (i.e. desired) @ContentType@ of the correspondent HTTP response
-- bodies.
data Codec = Codec ContentType ContentType

instance Show Codec where
    show (Codec content accept') =
        "(Accept: " ++ show accept' ++ ", Content-Type: " ++ show content ++ ")"

codecHeaders :: Codec -> [HTTP.Header]
codecHeaders (Codec content accept) = [contentTypeHeader content, acceptHeader accept]

contentTypeHeader, acceptHeader :: ContentType -> HTTP.Header
contentTypeHeader JSON = (HTTP.hContentType, "application/json")
contentTypeHeader CBOR = (HTTP.hContentType, "application/cbor")
acceptHeader      JSON = (HTTP.hAccept,      "application/json")
acceptHeader      CBOR = (HTTP.hAccept,      "application/cbor")

supportedContentType :: [HTTP.Header] -> Either Text ContentType
supportedContentType headers = case lookup HTTP.hContentType headers of
    Nothing -> Left $ "No Content-Type found in headers"
    Just ct -> parseContentType $ decodeUtf8 ct

encodeBody :: (ToJSON a, Serialise a) => ContentType -> a -> LBS.ByteString
encodeBody = encode

decodeBody :: (FromJSON a, Serialise a) => Wai.SResponse -> Either Text a
decodeBody resp = case supportedContentType (Wai.simpleHeaders resp) of
    Left err -> Left err
    Right ct -> decode ct $ Wai.simpleBody resp

get, delete :: ContentType -> Text -> Wai.Session Wai.SResponse
get = withoutBody GET
delete = withoutBody DELETE

put, post  :: (Aeson.ToJSON a, Serialise a) => ContentType -> Text -> a -> Wai.Session Wai.SResponse
put  = withBody PUT
post = withBody POST

withoutBody :: HTTP.StdMethod -> ContentType -> Text -> Wai.Session Wai.SResponse
withoutBody method ct path = request method path [acceptHeader ct] noBody

withBody :: (Aeson.ToJSON a, Serialise a) => HTTP.StdMethod -> ContentType -> Text -> a -> Wai.Session Wai.SResponse
withBody method ct path body = request method path headers $ encode ct body
    where headers = [contentTypeHeader ct, acceptHeader ct]

-- | Represents an empty request body.
noBody :: LBS.ByteString
noBody = LBS.empty

-- | Helper for string literals.
t :: Text -> Text
t = identity

responseBody :: Wai.SResponse -> LBS.ByteString
responseBody = Wai.simpleBody
