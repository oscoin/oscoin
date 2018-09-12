{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: Move this module to Network.Wai.Test.Extended
module Oscoin.Test.HTTP.Helpers where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Consensus.Evaluator (identityEval)
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Crypto.Blockchain (Blockchain(..), blocks)
import           Oscoin.Crypto.Blockchain.Block (toOrphan, emptyGenesisBlock)
import           Oscoin.Environment
import qualified Oscoin.API.Types as API
import           Oscoin.API.HTTP (api)
import           Oscoin.API.HTTP.Internal (mkMiddleware, decode, encode, parseContentType, ContentType(..))
import qualified Oscoin.Node as Node
import qualified Oscoin.Logging as Log
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Data.Tx (mkTx)

import           Oscoin.Test.Data.Rad.Arbitrary ( )
import           Oscoin.Test.Consensus.Node (DummyNodeId)

import           Test.QuickCheck (generate, arbitrary)
import           Test.Tasty.HUnit (Assertion, AssertionPredicable)
import qualified Test.Tasty.HUnit as Tasty

import           Crypto.Random.Types (MonadRandom(..))
import           Data.List (lookup)
import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON, FromJSON)
import           Codec.Serialise (Serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import qualified Network.HTTP.Types.Header as HTTP
import           Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import           Web.Spock (spockAsApp)


-- | Like "Assertion" but bound to a user session (cookies etc.)
type Session = Wai.Session

-- | Node handle for API tests.
type NodeHandle = Node.Handle API.RadTx Rad.Env DummyNodeId

-- | Node state to instantiate a NodeHandle with.
data NodeState = NodeState
    { mempoolState    :: [API.RadTx]
    , blockstoreState :: Blockchain API.RadTx Rad.Env
    }

instance Semigroup a => Semigroup (Session a) where
    (<>) = liftA2 (<>)

instance (Monoid a, Semigroup a) => Monoid (Session a) where
    mempty = pure mempty
    mappend = (<>)

instance MonadRandom Session where
    getRandomBytes = io . getRandomBytes

emptyNodeState :: NodeState
emptyNodeState = NodeState { mempoolState = mempty, blockstoreState = emptyBlockstore }

nodeState :: [API.RadTx] -> Blockchain API.RadTx Rad.Env -> NodeState
nodeState mp bs = NodeState { mempoolState = mp, blockstoreState = bs }

emptyBlockstore :: Blockchain API.RadTx Rad.Env
emptyBlockstore = Blockchain $ emptyGenesisBlock 0 :| []

makeNode :: NodeState -> IO NodeHandle
makeNode NodeState{..} = do
    let cfg = Node.Config { Node.cfgEnv = Testing , Node.cfgLogger = Log.noLogger }

    mp <- atomically $ do
        mp' <- Mempool.new
        Mempool.insertMany mp' mempoolState
        pure $ mp'

    bs <- atomically $ do
        bs' <- BlockStore.new $ genesisBlockStore $ emptyGenesisBlock @API.RadTx 0
        forM_ (blocks blockstoreState) $ \b ->
            let orphaned = toOrphan identityEval b
            in BlockStore.put bs' orphaned
        pure $ bs'

    st <- STree.new

    Node.open cfg 42 mp st bs

genDummyTx :: IO (Text, API.RadTx)
genDummyTx = do
    msg :: Rad.Value <- generate $ arbitrary
    (pubKey, priKey) <- Crypto.generateKeyPair
    signed           <- Crypto.sign priKey msg

    let tx :: API.RadTx = mkTx signed pubKey
    let txHash          = decodeUtf8 $ Crypto.toHex $ Crypto.hash tx

    pure $ (txHash, tx)

-- | Turn a "Session" into an "Assertion".
runSession :: Session () -> NodeHandle -> Assertion
runSession sess nh = do
    app <- spockAsApp (mkMiddleware (api Testing) nh)
    Wai.runSession sess app

infix 1 @?=, @=?, @?

(@?=) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(@?=) actual expected = io $ (Tasty.@?=) actual expected

(@=?) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(@=?) = flip (@?=)

(@?) :: (MonadIO m, AssertionPredicable t, HasCallStack) => t -> String -> m ()
(@?) predi msg = io $ (Tasty.@?) predi msg


assertStatus :: HasCallStack => HTTP.Status -> Wai.SResponse -> Wai.Session ()
assertStatus expected response = io $ Tasty.assertBool msg (actual == expected)
  where
    actual = Wai.simpleStatus response
    msg = concat
        [ "Expected status code "
        , show expected
        , ", but received "
        , show actual
        ]

-- | Assert that the response can be deserialised to @API.Ok actual@
-- and @actual@ equals @expected@.
assertResultOK
    :: (HasCallStack, FromJSON a, Serialise a, Eq a, Show a)
    => a -> Wai.SResponse -> Wai.Session ()
assertResultOK expected response = do
    result <- assertResponseBody response
    case result of
        API.Err err -> io $ Tasty.assertFailure $ "Received API error: " <> T.unpack err
        API.Ok v -> expected @=? v

-- | Assert that the response can be deserialised to @API.Err actual@
-- and @actual@ equals @expected@.
assertResultErr
    :: (HasCallStack)
    => Text -> Wai.SResponse -> Wai.Session ()
assertResultErr expected response = do
    result <- assertResponseBody @(API.Result ()) response
    case result of
        API.Err err -> expected @=? err
        API.Ok _ -> io $ Tasty.assertFailure $ "Received unexpected API OK result"

assertResponseBody
    :: (HasCallStack, FromJSON a, Serialise a)
    => Wai.SResponse -> Wai.Session a
assertResponseBody response =
    case responseBody response of
        Left err -> io $ Tasty.assertFailure $ show err
        Right a -> pure $ a


-- | Low-level HTTP request helper.
request
    :: (HasCallStack, ToJSON a, Serialise a)
    => HTTP.StdMethod                    -- ^ Request method
    -> Text                              -- ^ Request path
    -> HTTP.RequestHeaders               -- ^ Request headers
    -> Maybe a                           -- ^ Request body
    -> Wai.Session Wai.SResponse
request method (encodeUtf8 -> path) headers mb
    | Nothing <- mb = req LBS.empty
    | Just b  <- mb = case supportedContentType headers of
        Left err -> io $ Tasty.assertFailure $ show err
        Right ct -> req $ encode ct b
    where req = Wai.srequest . Wai.SRequest (mkRequest method path headers)


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

responseBody :: (FromJSON a, Serialise a) => Wai.SResponse -> Either Text a
responseBody resp = case supportedContentType (Wai.simpleHeaders resp) of
    Left err -> Left err
    Right ct -> decode ct $ Wai.simpleBody resp

get :: HasCallStack => Codec -> Text -> Wai.Session Wai.SResponse
get = withoutBody GET

post
    :: (HasCallStack, Aeson.ToJSON a, Serialise a)
    => Codec -> Text -> a -> Wai.Session Wai.SResponse
post = withBody POST

withoutBody :: HasCallStack => HTTP.StdMethod -> Codec -> Text -> Wai.Session Wai.SResponse
withoutBody method (Codec _ acceptCt) path =
    request method path [acceptHeader acceptCt] noBody

withBody
    :: (HasCallStack, Aeson.ToJSON a, Serialise a)
    => HTTP.StdMethod -> Codec -> Text -> a -> Wai.Session Wai.SResponse
withBody method codec path body' =
    request method path (codecHeaders codec) $ Just body'

-- | Represents an empty request body.
noBody :: Maybe ()
noBody = Nothing

-- | Helper for string literals.
t :: Text -> Text
t = identity
