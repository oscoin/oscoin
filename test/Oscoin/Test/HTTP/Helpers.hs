{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.HTTP.Helpers
    ( NodeState(..)
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

    , addRef
    , initEnv
    , genDummyTx
    , createValidTx
    ) where

import           Oscoin.Prelude hiding (First, get)

import qualified Oscoin.API.HTTP as API
import qualified Oscoin.API.Types as API
import           Oscoin.Configuration (Environment(Development))
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Trivial (blockScore, trivialConsensus)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock, sealBlock)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.P2P.Types as P2P (fromPhysicalNetwork, randomNetwork)
import           Oscoin.Protocol (runProtocol)
import           Oscoin.Storage.Block.Memory
import qualified Oscoin.Storage.Ledger as Ledger
import qualified Oscoin.Telemetry as Telemetry
import qualified Oscoin.Telemetry.Logging as Log
import qualified Oscoin.Telemetry.Metrics as Metrics
import           Oscoin.Time

import           Oscoin.Test.Consensus.Network (DummyNodeId)
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Helpers
                 (defaultBeneficiary)
import           Oscoin.Test.Data.Tx.Arbitrary ()

import           Test.QuickCheck (arbitrary)
import           Test.QuickCheck.Monadic
import           Test.Tasty.HUnit.Extended

import           Codec.Serialise
import           Control.Monad.Fail (MonadFail(..))
import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as T
import           System.Random.SplitMix (newSMGen)

import qualified Network.HTTP.Types.Header as HTTP
import           Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai

-- | The 'Session' monad allows for arbitrary IO, communication with
-- the Node HTTP API (via 'Wai.Session') and access to the underlying
-- node directly.
--
-- See also 'liftWaiSession' and 'liftNode'.
newtype Session c a = Session (ReaderT (NodeHandle c) Wai.Session a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFail (Session c) where
    fail = assertFailure

type DummySeal = Text

-- | Node handle for API tests.
type Node c = Node.NodeT c (Tx c) DummySeal DummyNodeId IO

-- | Node handle for API tests.
type NodeHandle c = Node.Handle c (Tx c) DummySeal DummyNodeId

-- | Node state to instantiate a NodeHandle with.
data NodeState c = NodeState
    { mempoolState    :: [Tx c]
    , blockstoreState :: Blockchain c (Tx c) DummySeal
    , statestoreState :: TxState c (Tx c)
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
    , statestoreState = mempty
    }

nodeState
    :: [Tx c]
    -> Blockchain c (Tx c) DummySeal
    -> TxState c (Tx c)
    -> NodeState c
nodeState mp bs st = NodeState { mempoolState = mp, blockstoreState = bs, statestoreState = st }

withNode :: IsCrypto c => NodeState c -> (NodeHandle c -> IO a) -> IO a
withNode NodeState{..} k = do
    let env    = Development
    let logger = Log.noLogger
    metrics   <-
        Telemetry.newTelemetryStore logger <$>
            Metrics.newMetricsStore Metrics.noLabels
    net       <- P2P.randomNetwork <$> newSMGen
    let config = Consensus.configForEnvironment env
    let cfg    = Node.Config
            { Node.cfgGlobalConfig    = Node.GlobalConfig
                { Node.globalEnv             = env
                , Node.globalLogicalNetwork  = P2P.fromPhysicalNetwork net
                , Node.globalPhysicalNetwork = net
                }
            , Node.cfgTelemetry       = metrics
            , Node.cfgConsensusConfig = config
            , Node.cfgBeneficiary     = defaultBeneficiary
            }

    mph <- atomically $ do
        mp <- Mempool.new validateTx
        forM_ mempoolState $ Mempool.insert mp
        pure mp

    blkStore@(blockStoreReader, _) <- newBlockStoreIO (blocks' blockstoreState)
    let dummyEval _ s = Right ([], s)
    ledger <- Ledger.newFromBlockStoreIO dummyEval blockStoreReader statestoreState
    runProtocol (\_ _ -> Right ()) blockScore metrics blkStore config $ \dispatchBlock ->
        Node.withNode
            cfg
            42
            mph
            ledger
            dispatchBlock
            (trivialConsensus "")
            k

liftNode :: Node c a -> Session c a
liftNode na = Session $ ReaderT $ \h -> liftIO (Node.runNodeT h na)

genDummyTx
    :: (IsCrypto c, MonadIO m)
    => PropertyM m (Hashed c (Tx c), Tx c)
genDummyTx = do
    txVal <- pick arbitrary
    createValidTx txVal

createValidTx
    :: forall c m.
       (IsCrypto c, MonadIO m)
    => TxPayload c (Tx c)
    -> m (Hashed c (Tx c), Tx c)
createValidTx payload = liftIO $ do
    (pubKey, priKey) <- Crypto.generateKeyPair
    signed           <- Crypto.sign priKey payload

    let tx :: (Tx c) = mkTx signed pubKey
    let txHash       = Crypto.hash tx

    pure (txHash, tx)

-- | Creates a new empty blockchain with a dummy seal.
emptyBlockchain :: IsCrypto c => Blockchain c (Tx c) DummySeal
emptyBlockchain = fromGenesis $ sealBlock "" (emptyGenesisBlock epoch defaultBeneficiary)

initEnv :: [(Text, DummyPayload)] -> TxState c (Tx c)
initEnv bindings =
    DummyEnv $ foldl' (\acc (k,v) -> Map.insert k v acc) mempty bindings

-- | Adds a reference holding @value@ and binds @name@ to the reference
addRef :: Text -> DummyPayload -> DummyEnv -> DummyEnv
addRef name value (DummyEnv env) = DummyEnv $ Map.insert name value env

-- | Run a 'Session' with the given initial node state
runSession :: IsCrypto c => NodeState c -> Session c () -> Assertion
runSession nst (Session sess) =
    withNode nst $ \nh -> do
        app <- API.app nh
        Wai.runSession (runReaderT sess nh) app

-- | Run a 'Session' so that the blockchain state is the given environment.
runSessionEnv :: IsCrypto c => DummyEnv -> Session c () -> Assertion
runSessionEnv env (Session sess) = do
    let nst = nodeState [] (fromGenesis $ sealBlock "" $ emptyGenesisFromState epoch defaultBeneficiary env) env
    withNode nst $ \nh -> do
        app <- API.app nh
        Wai.runSession (runReaderT sess nh) app


-- | Run a 'Session' so that the blockchain state has the given
-- Radicle bindings.
runSessionBindings :: IsCrypto c => [(Text, DummyPayload)] -> Session c () -> Assertion
runSessionBindings bindings = runSessionEnv (initEnv bindings)

-- | Run a 'Session' with an empty node state. That is the node mempool
-- is empty and the blockchain has only the genesis block with a pure
-- Radicle environment.
runEmptySession :: IsCrypto c => Session c () -> Assertion
runEmptySession = runSessionEnv mempty


assertStatus :: HasCallStack => HTTP.Status -> Wai.SResponse -> Session c ()
assertStatus want (Wai.simpleStatus -> have) = have @?= want

-- | Assert that the response can be deserialised to @API.Ok actual@
-- and @actual@ equals @expected@.
assertResultOK
    :: (HasCallStack, Serialise a, Eq a, Show a)
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
    :: (HasCallStack, Serialise a)
    => Wai.SResponse -> Session c a
assertResponseBody response =
    case responseBody response of
        Left err -> assertFailure $ show err
        Right a  -> pure $ a
  where
    responseBody :: (Serialise a) => Wai.SResponse -> Either Text a
    responseBody resp =
        first (T.pack . show) (deserialiseOrFail $ Wai.simpleBody resp)


-- | Low-level HTTP request helper.
request
    :: (Serialise a)
    => HTTP.StdMethod                    -- ^ Request method
    -> Text                              -- ^ Request path
    -> Maybe a                           -- ^ Request body
    -> Session c Wai.SResponse
request method path maybeBody =
    liftWaiSession $ Wai.srequest $ Wai.SRequest req bodyData
  where
    bodyData = case maybeBody of
        Just a  -> serialise a
        Nothing -> LBS.empty
    headers = case maybeBody of
        Just _  -> [(HTTP.hContentType, "application/cbor"), acceptHeader]
        Nothing -> [acceptHeader]
    acceptHeader = (HTTP.hAccept, "application/cbor")
    req =
        Wai.setPath
            Wai.defaultRequest
                { Wai.requestMethod = HTTP.renderStdMethod method
                , Wai.requestHeaders = headers
                }
            (encodeUtf8 path)


get :: Text -> Session c Wai.SResponse
get path = request GET path (Nothing @())

post :: (Serialise a) => Text -> a -> Session c Wai.SResponse
post path body = request POST path (Just body)
