{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.HTTP.Helpers
    ( Codec
    , newCodec
    , prettyCodec

    , NodeState(..)
    , nodeState
    , emptyNodeState

    , Session
    , runSession
    , runEmptySession
    , runSessionBindings
    , runSessionEnv
    , liftWaiSession
    , liftNode

    , assertResultOK
    , assertResultErr
    , assertStatus

    , get
    , post

    , addRadicleRef
    , initRadicleEnv
    , genDummyTx
    , createValidTx
    ) where

import           Oscoin.Prelude hiding (First, get)

import qualified Oscoin.API.HTTP as API
import           Oscoin.API.HTTP.Internal
                 (MediaType(..), decode, encode, parseMediaType)
import qualified Oscoin.API.Types as API
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Trivial (blockScore, trivialConsensus)
import           Oscoin.Crypto.Blockchain (Blockchain(..), fromGenesis)
import           Oscoin.Crypto.Blockchain.Block
                 (emptyGenesisBlock, genesisBlock, sealBlock)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Data.RadicleTx as Rad
import           Oscoin.Data.Tx (mkTx)
import           Oscoin.Environment
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Storage.Block.STM as BlockStore.Concrete.STM
import qualified Oscoin.Storage.State as StateStore
import qualified Oscoin.Telemetry as Telemetry
import qualified Oscoin.Telemetry.Logging as Log
import qualified Oscoin.Telemetry.Metrics as Metrics
import           Oscoin.Time

import           Oscoin.Test.Consensus.Node (DummyNodeId, DummySeal)
import           Oscoin.Test.Crypto
import           Oscoin.Test.Data.Rad.Arbitrary ()

import           Test.QuickCheck (arbitrary)
import           Test.QuickCheck.Monadic
import           Test.Tasty.HUnit.Extended

import           Codec.Serialise (Serialise)
import           Control.Monad.Fail (MonadFail(..))
import           Crypto.Random.Types (MonadRandom(..))
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List (lookup)
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

-- | The 'Session' monad allows for arbitrary IO, communication with
-- the Node HTTP API (via 'Wai.Session') and access to the underlying
-- node directly.
--
-- See also 'liftWaiSession' and 'liftNode'.
newtype Session c a = Session (ReaderT (NodeHandle c) Wai.Session a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFail (Session c) where
    fail = assertFailure

-- | Node handle for API tests.
type Node c = Node.NodeT c (API.RadTx c) (Rad.Env c) DummySeal DummyNodeId IO

-- | Node handle for API tests.
type NodeHandle c = Node.Handle c (API.RadTx c) (Rad.Env c) DummySeal DummyNodeId

-- | Node state to instantiate a NodeHandle with.
data NodeState c = NodeState
    { mempoolState    :: [API.RadTx c]
    , blockstoreState :: Blockchain c (API.RadTx c) DummySeal
    , statestoreState :: Rad.Env c
    }

instance Semigroup a => Semigroup (Session c a) where
    (<>) = liftA2 (<>)

instance (Monoid a, Semigroup a) => Monoid (Session c a) where
    mempty = pure mempty
    mappend = (<>)

instance MonadRandom (Session c) where
    getRandomBytes = liftIO . getRandomBytes

liftWaiSession :: Wai.Session a -> Session c a
liftWaiSession s = Session $ lift s

emptyNodeState :: IsCrypto c => NodeState c
emptyNodeState = NodeState
    { mempoolState = mempty
    , blockstoreState = emptyBlockchain
    , statestoreState = Rad.pureEnv
    }

nodeState
    :: [API.RadTx c]
    -> Blockchain c (API.RadTx c) DummySeal
    -> Rad.Env c
    -> NodeState c
nodeState mp bs st = NodeState { mempoolState = mp, blockstoreState = bs, statestoreState = st }

withNode :: IsCrypto c => NodeState c -> (NodeHandle c -> IO a) -> IO a
withNode NodeState{..} k = do
    let env    = Testing
        logger = Log.noLogger
    config <- Consensus.getConfig env
    metricsStore <- Metrics.newMetricsStore Metrics.noLabels
    let handle = Telemetry.newTelemetryStore logger metricsStore
    let cfg = Node.Config { Node.cfgEnv = env
                          , Node.cfgTelemetry = handle
                          , Node.cfgNoEmptyBlocks = False
                          , Node.cfgConsensusConfig = config
                          }

    mph <- atomically $ do
        mp <- Mempool.new
        Mempool.insertMany mp mempoolState
        pure mp

    BlockStore.Concrete.STM.withBlockStore blockstoreState blockScore $ \bsh -> do
        sth <- liftIO $ StateStore.fromStateM statestoreState

        Node.withNode
            cfg
            42
            mph
            sth
            bsh
            Rad.txEval
            (trivialConsensus "")
            k

liftNode :: Node c a -> Session c a
liftNode na = Session $ ReaderT $ \h -> liftIO (Node.runNodeT h na)

genDummyTx
    :: (IsCrypto c, MonadIO m)
    => PropertyM m (Hashed c (API.RadTx c), API.RadTx c)
genDummyTx = do
    radValue <- pick arbitrary
    createValidTx radValue

createValidTx
    :: forall c m.
       (IsCrypto c, MonadIO m)
    => Rad.Value
    -> m (Hashed c (API.RadTx c), API.RadTx c)
createValidTx radValue = liftIO $ do
    (pubKey, priKey) <- Crypto.generateKeyPair
    signed           <- Crypto.sign priKey radValue

    let tx :: (API.RadTx c) = mkTx signed pubKey
    let txHash              = Crypto.hash tx

    pure (txHash, tx)

-- | Creates a new empty blockchain with a dummy seal.
emptyBlockchain :: IsCrypto c => Blockchain c (API.RadTx c) DummySeal
emptyBlockchain = fromGenesis $ sealBlock "" (emptyGenesisBlock epoch)

-- | Create a Radicle environment with the given bindings
initRadicleEnv :: [(Text, Radicle.Value)] -> Rad.Env c
initRadicleEnv bindings =
    Rad.Env $ foldl' addBinding Radicle.pureEnv bindings
  where
    addBinding env (id, value) = Radicle.addBinding (Radicle.unsafeToIdent id) value env

-- | Adds a reference holding @value@ and binds @name@ to the reference
addRadicleRef :: Text -> Radicle.Value -> Rad.Env c -> Rad.Env c
addRadicleRef name value (Rad.Env env) =
    Rad.Env env'
  where
    env' = snd $ runIdentity $ Radicle.runLang env $ do
        ref <- Radicle.newRef value
        Radicle.defineAtom (Radicle.unsafeToIdent name) ref


-- | Run a 'Session' with the given initial node state
runSession :: IsCrypto c => NodeState c -> Session c () -> Assertion
runSession nst (Session sess) =
    withNode nst $ \nh -> do
        app <- API.app nh
        Wai.runSession (runReaderT sess nh) app

-- | Run a 'Session' so that the blockchain state is the given
-- Radicle environment
runSessionEnv :: IsCrypto c => Rad.Env c -> Session c () -> Assertion
runSessionEnv env (Session sess) = do
    let nst = nodeState [] (fromGenesis $ genesisBlock [] env "" epoch) env
    withNode nst $ \nh -> do
        app <- API.app nh
        Wai.runSession (runReaderT sess nh) app


-- | Run a 'Session' so that the blockchain state has the given
-- Radicle bindings.
runSessionBindings :: IsCrypto c => [(Text, Rad.Value)] -> Session c () -> Assertion
runSessionBindings bindings = runSessionEnv (initRadicleEnv bindings)

-- | Run a 'Session' with an empty node state. That is the node mempool
-- is empty and the blockchain has only the genesis block with a pure
-- Radicle environment.
runEmptySession :: IsCrypto c => Session c () -> Assertion
runEmptySession = runSessionEnv Rad.pureEnv


assertStatus :: HasCallStack => HTTP.Status -> Wai.SResponse -> Session c ()
assertStatus want (Wai.simpleStatus -> have) = have @?= want

-- | Assert that the response can be deserialised to @API.Ok actual@
-- and @actual@ equals @expected@.
assertResultOK
    :: (HasCallStack, FromJSON a, Serialise a, Eq a, Show a)
    => a -> Wai.SResponse -> Session c ()
assertResultOK expected response = do
    result <- assertResponseBody response
    case result of
        API.Err err -> assertFailure $ "Received API error: " <> T.unpack err
        API.Ok v    -> expected @=? v

-- | Assert that the response can be deserialised to @API.Err actual@
-- and @actual@ equals @expected@.
assertResultErr
    :: (HasCallStack)
    => Text -> Wai.SResponse -> Session c ()
assertResultErr expected response = do
    result <- assertResponseBody @(API.Result ()) response
    case result of
        API.Err err -> expected @=? err
        API.Ok _    -> assertFailure $ "Received unexpected API OK result"

assertResponseBody
    :: (HasCallStack, FromJSON a, Serialise a)
    => Wai.SResponse -> Session c a
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
    -> Session c Wai.SResponse
request method (encodeUtf8 -> path) headers mb
    | Nothing <- mb = liftWaiSession $ req LBS.empty
    | Just b  <- mb = case supportedContentType headers of
        Left err -> assertFailure $ show err
        Right ct -> liftWaiSession $ req $ encode ct b
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

get :: HasCallStack => Codec -> Text -> Session c Wai.SResponse
get = withoutBody GET

post
    :: (HasCallStack, Aeson.ToJSON a, Serialise a)
    => Codec -> Text -> a -> Session c Wai.SResponse
post = withBody POST

withoutBody :: HasCallStack => HTTP.StdMethod -> Codec -> Text -> Session c Wai.SResponse
withoutBody method Codec{..} path =
    request method path [acceptHeader codecAccept] noBody

withBody
    :: (HasCallStack, Aeson.ToJSON a, Serialise a)
    => HTTP.StdMethod -> Codec -> Text -> a -> Session c Wai.SResponse
withBody method codec path body' =
    request method path (codecHeaders codec) $ Just body'

-- | Represents an empty request body.
noBody :: Maybe ()
noBody = Nothing
