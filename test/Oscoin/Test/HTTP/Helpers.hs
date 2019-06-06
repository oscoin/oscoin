{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.HTTP.Helpers
    ( DummySeal
    , NodeState(..)
    , nodeState
    , emptyNodeState

    , Session
    , runSession
    , runEmptySession
    , runSessionWithState
    , runSessionWithState'
    , liftWaiSession
    , liftNode
    , withNode

    , assertResultOK
    , assertStatus

    , get
    , post

    , genDummyTx
    , createValidTx
    ) where

import           Oscoin.Prelude hiding (First, get)

import qualified Oscoin.API.HTTP as API
import qualified Oscoin.API.Types as API
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Trivial (blockScore, trivialConsensus)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx hiding (validateTx)
import qualified Oscoin.Data.Tx as Tx
import           Oscoin.Data.Tx.Abstract as Abstract
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.P2P.Types as P2P (fromPhysicalNetwork, randomNetwork)
import           Oscoin.Protocol (runProtocol)
import           Oscoin.Storage.Block.Memory
import qualified Oscoin.Storage.Ledger as Ledger
import qualified Oscoin.Telemetry as Telemetry
import qualified Oscoin.Telemetry.Logging as Log
import qualified Oscoin.Telemetry.Metrics as Metrics

import           Oscoin.Test.Consensus.Network (DummyNodeId)
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
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
newtype Session c tx s a = Session (ReaderT (NodeHandle c tx s) Wai.Session a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadFail (Session c tx s) where
    fail = assertFailure

type DummySeal = Text

-- | Node handle for API tests.
type Node c = Node.NodeT c (Tx c) DummySeal DummyNodeId IO

-- | Node handle for API tests.
type NodeHandle c tx s = Node.Handle c tx s DummyNodeId

-- | Node state to instantiate a NodeHandle with.
data NodeState c tx s = NodeState
    { mempoolState    :: [tx]
    , blockstoreState :: Blockchain c tx s
    , statestoreState :: TxState c tx
    }

instance Semigroup a => Semigroup (Session c tx s a) where
    (<>) = liftA2 (<>)

instance (Monoid a, Semigroup a) => Monoid (Session c tx s a) where
    mempty = pure mempty
    mappend = (<>)

instance MonadRandom (Session c tx s) where
    getRandomBytes = liftIO . getRandomBytes

liftWaiSession :: Wai.Session a -> Session c tx s a
liftWaiSession s = Session $ lift s

emptyNodeState :: IsCrypto c => NodeState c (Tx c) DummySeal
emptyNodeState = NodeState
    { mempoolState = mempty
    , blockstoreState = emptyBlockchain
    , statestoreState = mempty
    }

nodeState
    :: [tx]
    -> Blockchain c tx s
    -> TxState c tx
    -> NodeState c tx s
nodeState mp bs st = NodeState { mempoolState = mp, blockstoreState = bs, statestoreState = st }

withNode
    :: forall c tx s a.
       ( IsCrypto c
       , Serialise s
       , Ord s
       , Hashable c (Abstract.TxState c tx)
       , Hashable c tx
       )
    => Evaluator c (TxState c tx) tx (TxOutput c tx)
    -- ^ The empyt transaction output
    -> (tx -> Either (Abstract.TxValidationError c tx) ())
    -- ^ A validation function for the mempool.
    -> s
    -- ^ The seal
    -> NodeState c tx s
    -> (NodeHandle c tx s -> IO a)
    -> IO a
withNode eval validateTx seal NodeState{..} k = do
    let logger = Log.noLogger
    metrics   <-
        Telemetry.newTelemetryStore logger <$>
            Metrics.newMetricsStore Metrics.noLabels
    net       <- P2P.randomNetwork <$> newSMGen
    let config = Consensus.testConfig
    let cfg    = Node.Config
            { Node.cfgGlobalConfig    = Node.GlobalConfig
                { Node.globalLogicalNetwork  = P2P.fromPhysicalNetwork net
                , Node.globalPhysicalNetwork = net
                }
            , Node.cfgTelemetry       = metrics
            , Node.cfgConsensusConfig = config
            , Node.cfgBeneficiary     = someBeneficiary
            }

    mph <- atomically $ do
        mp <- Mempool.new validateTx
        forM_ mempoolState $ Mempool.insert mp
        pure mp

    blkStore@(blockStoreReader, _) <- newBlockStoreIO (blocks' blockstoreState)
    ledger <- Ledger.newFromBlockStoreIO eval blockStoreReader statestoreState
    runProtocol (\_ _ -> Right ()) blockScore metrics blkStore config $ \dispatchBlock ->
        Node.withNode
            cfg
            42
            mph
            ledger
            dispatchBlock
            (trivialConsensus seal)
            k

liftNode :: Node c a -> Session c (Tx c) DummySeal a
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
    => DummyPayload
    -> m (Hashed c (Tx c), Tx c)
createValidTx payload = liftIO $ do
    (pubKey, priKey) <- Crypto.generateKeyPair
    signed           <- Crypto.sign priKey payload

    let tx :: (Tx c) = mkTx signed pubKey
    let txHash       = Crypto.hash tx

    pure (txHash, tx)

-- | Creates a new empty blockchain with a dummy seal.
emptyBlockchain :: IsCrypto c => Blockchain c tx DummySeal
emptyBlockchain = fromGenesis $ someGenesisBlock ""

-- | Run a 'Session' with the given initial node state
runSession
    :: IsCrypto c
    => NodeState c (Tx c) DummySeal
    -> Session c (Tx c) DummySeal ()
    -> Assertion
runSession nst (Session sess) =
    withNode Tx.evaluateBlock Tx.validateTx mempty nst $ \nh -> do
        app <- API.app nh
        Wai.runSession (runReaderT sess nh) app

-- | Run a 'Session' so that the blockchain state has the given
-- Radicle bindings.
runSessionWithState
    :: ( IsCrypto c
       , MonadIO m
       , Serialise s
       , Ord s
       )
    => s
    -- ^ A seal for the genesis block.
    -> [(ByteString, ByteString)]
    -> Session c (Tx c) s ()
    -> m ()
runSessionWithState seal bindings (Session sess)= do
    let initialState = LegacyTxState $ Map.fromList bindings
    let genBlock = someGenesisBlock' seal $ Crypto.fromHashed $ Crypto.hash initialState
    let nst = nodeState [] (fromGenesis genBlock) initialState
    liftIO $ withNode Tx.evaluateBlock Tx.validateTx seal nst $ \nh -> do
        app <- API.app nh
        Wai.runSession (runReaderT sess nh) app

-- | Like 'runSessionWithState', but monomorphic in the seal and in the
-- transaction type.
runSessionWithState'
    :: ( IsCrypto c
       , MonadIO m
       )
    => [(ByteString, ByteString)]
    -> Session c (Tx c) DummySeal ()
    -> m ()
runSessionWithState' = runSessionWithState mempty

-- | Run a 'Session' with an empty node state. That is the node mempool
-- is empty and the blockchain has only the genesis block with an empty
-- dummy environment.
runEmptySession :: forall c m. (IsCrypto c, MonadIO m) => Session c (Tx c) DummySeal () -> m ()
runEmptySession = runSessionWithState' @c @m []


assertStatus :: HasCallStack => HTTP.Status -> Wai.SResponse -> Session c tx s ()
assertStatus want (Wai.simpleStatus -> have) = have @?= want

-- | Assert that the response can be deserialised to @API.Ok actual@
-- and @actual@ equals @expected@.
assertResultOK
    :: (HasCallStack, Serialise a, Eq a, Show a)
    => a -> Wai.SResponse -> Session c tx s ()
assertResultOK expected response = do
    result <- assertResponseBody response
    case result of
        API.Err err -> assertFailure $ "Received API error: " <> T.unpack err
        API.Ok v    -> expected @=? v

assertResponseBody
    :: (HasCallStack, Serialise a)
    => Wai.SResponse -> Session c tx s a
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
    -> Session c tx s Wai.SResponse
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


get :: Text -> Session c tx s Wai.SResponse
get path = request GET path (Nothing @())

post :: (Serialise a) => Text -> a -> Session c tx s Wai.SResponse
post path body = request POST path (Just body)
