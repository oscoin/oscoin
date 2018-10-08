{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.HTTP.Helpers
    ( Codec
    , newCodec
    , prettyCodec

    , NodeState(..)
    , nodeState
    , withNode
    , emptyNodeState

    , Session
    , runSession

    , assertResultOK
    , assertResultErr
    , assertStatus

    , get
    , post

    , blockchainFromEnv
    , addRadicleRef
    , initRadicleEnv
    , genDummyTx
    , emptyBlockstore

    ) where

import           Oscoin.Prelude hiding (First, get)

import qualified Oscoin.API.HTTP as API
import           Oscoin.API.HTTP.Internal
                 (MediaType(..), decode, encode, parseMediaType)
import qualified Oscoin.API.Types as API
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Consensus.BlockStore (BlockStore(..))
import qualified Oscoin.Consensus.BlockStore as BlockStore
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain
                 (Blockchain(..), blockHash, fromGenesis, height, mkBlock, tip)
import           Oscoin.Crypto.Blockchain.Block
                 (blockState, emptyGenesisBlock, emptyHeader)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (mkTx)
import           Oscoin.Environment
import qualified Oscoin.Logging as Log
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Time

import           Oscoin.Test.Consensus.Node (DummyNodeId)
import           Oscoin.Test.Data.Rad.Arbitrary ()

import           Test.QuickCheck (arbitrary, generate)
import           Test.Tasty.HUnit.Extended

import           Codec.Serialise (Serialise)
import           Crypto.Random.Types (MonadRandom(..))
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Default (def)
import           Data.List (lookup)
import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import           Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai

import qualified Radicle
-- FIXME(kim): should use unsafeToIdent, cf. radicle#105
import qualified Radicle.Internal.Core as Radicle


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
    getRandomBytes = liftIO . getRandomBytes

emptyNodeState :: NodeState
emptyNodeState = NodeState { mempoolState = mempty, blockstoreState = emptyBlockstore }

nodeState :: [API.RadTx] -> Blockchain API.RadTx Rad.Env -> NodeState
nodeState mp bs = NodeState { mempoolState = mp, blockstoreState = bs }

emptyBlockstore :: Blockchain API.RadTx Rad.Env
emptyBlockstore = Blockchain $ emptyGenesisBlock epoch def :| []

withNode :: NodeState -> (NodeHandle -> IO a) -> IO a
withNode NodeState{..} k = do
    let cfg = Node.Config { Node.cfgEnv = Testing , Node.cfgLogger = Log.noLogger }

    mph <- atomically $ do
        mp <- Mempool.new
        Mempool.insertMany mp mempoolState
        pure mp

    let bs = BlockStore
             { bsOrphans = mempty
             , bsChains = Map.singleton (blockHash $ tip $ blockstoreState) blockstoreState
             }

    bsh <- BlockStore.newIO bs
    sth <- STree.new (BlockStore.chainState (comparing height) bs)

    Node.withNode
        cfg
        42
        mph
        sth
        bsh
        Rad.txEval
        (Consensus.nakamotoConsensus (Just Nakamoto.easyDifficulty))
        k

genDummyTx :: IO (Hashed API.RadTx, API.RadTx)
genDummyTx = do
    msg :: Rad.Value <- generate $ arbitrary
    (pubKey, priKey) <- Crypto.generateKeyPair
    signed           <- Crypto.sign priKey msg

    let tx :: API.RadTx = mkTx signed pubKey
    let txHash          = Crypto.hash tx

    pure (txHash, tx)

-- | Creates a new Radicle blockchain with no transactions and the given state.
blockchainFromEnv :: Rad.Env -> Blockchain API.RadTx Rad.Env
blockchainFromEnv env = fromGenesis genesis
  where
    genesis = mkBlock (emptyHeader { blockState = env }) []

-- | Create a Radicle environment with the given bindings
initRadicleEnv :: [(Text, Radicle.Value)] -> Rad.Env
initRadicleEnv bindings =
    Rad.Env $ foldl' addBinding Radicle.pureEnv bindings
  where
    addBinding env (id, value) = Radicle.addBinding (Radicle.unsafeToIdent id) value env

-- | Adds a reference holding @value@ and binds @name@ to the reference
addRadicleRef :: Text -> Radicle.Value -> Rad.Env -> Rad.Env
addRadicleRef name value (Rad.Env env) =
    Rad.Env env'
  where
    env' = snd $ runIdentity $ Radicle.runLang env $ do
        ref <- Radicle.newRef value
        Radicle.defineAtom (Radicle.unsafeToIdent name) ref


-- | Turn a "Session" into an "Assertion".
runSession :: Session () -> NodeHandle -> Assertion
runSession sess nh = do
    app <- API.app Testing nh
    Wai.runSession sess app


assertStatus :: HasCallStack => HTTP.Status -> Wai.SResponse -> Wai.Session ()
assertStatus want (Wai.simpleStatus -> have) = have @?= want

-- | Assert that the response can be deserialised to @API.Ok actual@
-- and @actual@ equals @expected@.
assertResultOK
    :: (HasCallStack, FromJSON a, Serialise a, Eq a, Show a)
    => a -> Wai.SResponse -> Wai.Session ()
assertResultOK expected response = do
    result <- assertResponseBody response
    case result of
        API.Err err -> assertFailure $ "Received API error: " <> T.unpack err
        API.Ok v    -> expected @=? v

-- | Assert that the response can be deserialised to @API.Err actual@
-- and @actual@ equals @expected@.
assertResultErr
    :: (HasCallStack)
    => Text -> Wai.SResponse -> Wai.Session ()
assertResultErr expected response = do
    result <- assertResponseBody @(API.Result ()) response
    case result of
        API.Err err -> expected @=? err
        API.Ok _    -> assertFailure $ "Received unexpected API OK result"

assertResponseBody
    :: (HasCallStack, FromJSON a, Serialise a)
    => Wai.SResponse -> Wai.Session a
assertResponseBody response =
    case responseBody response of
        Left err -> assertFailure $ show err
        Right a  -> pure $ a


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
        Left err -> assertFailure $ show err
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
data Codec = Codec
    { codecAccept      :: HTTP.MediaType
    , codecContentType :: HTTP.MediaType
    } deriving (Show)

prettyCodec :: Codec -> Text
prettyCodec Codec{..} =
    "(Accept: " <> show codecAccept <>
    ", Content-Type: " <> show codecContentType <> ")"

newCodec :: HTTP.MediaType -> HTTP.MediaType -> Codec
newCodec accept ctype = Codec
    { codecAccept = accept
    , codecContentType = ctype
    }

codecHeaders :: Codec -> [HTTP.Header]
codecHeaders Codec{..} =
    [ contentTypeHeader codecContentType
    , acceptHeader      codecAccept
    ]

contentTypeHeader, acceptHeader :: HTTP.MediaType -> HTTP.Header
contentTypeHeader = (HTTP.hContentType, ) . mediaTypeBytes
acceptHeader      = (HTTP.hAccept, )      . mediaTypeBytes

mediaTypeBytes :: HTTP.MediaType -> BS.ByteString
mediaTypeBytes = encodeUtf8 . T.pack . show

supportedContentType :: [HTTP.Header] -> Either Text MediaType
supportedContentType headers = case lookup HTTP.hContentType headers of
    Nothing -> Left $ "No Content-Type found in headers"
    Just ct -> parseMediaType ct

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
withoutBody method Codec{..} path =
    request method path [acceptHeader codecAccept] noBody

withBody
    :: (HasCallStack, Aeson.ToJSON a, Serialise a)
    => HTTP.StdMethod -> Codec -> Text -> a -> Wai.Session Wai.SResponse
withBody method codec path body' =
    request method path (codecHeaders codec) $ Just body'

-- | Represents an empty request body.
noBody :: Maybe ()
noBody = Nothing
