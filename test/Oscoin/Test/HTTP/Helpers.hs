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
import           Oscoin.Configuration (Environment(Development))
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Trivial (blockScore, trivialConsensus)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock, sealBlock)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx
import           Oscoin.Data.Tx.Abstract
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
newtype Session c s a = Session (ReaderT (NodeHandle c s) Wai.Session a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadFail (Session c s) where
    fail = assertFailure

type DummySeal = Text

-- | Node handle for API tests.
type Node c = Node.NodeT c (Tx c) DummySeal DummyNodeId IO

-- | Node handle for API tests.
type NodeHandle c s = Node.Handle c (Tx c) s DummyNodeId

-- | Node state to instantiate a NodeHandle with.
data NodeState c s = NodeState
    { mempoolState    :: [Tx c]
    , blockstoreState :: Blockchain c (Tx c) s
    , statestoreState :: TxState c (Tx c)
    }

instance Semigroup a => Semigroup (Session c s a) where
    (<>) = liftA2 (<>)

instance (Monoid a, Semigroup a) => Monoid (Session c s a) where
    mempty = pure mempty
    mappend = (<>)

instance MonadRandom (Session c s) where
    getRandomBytes = liftIO . getRandomBytes

liftWaiSession :: Wai.Session a -> Session c s a
liftWaiSession s = Session $ lift s

emptyNodeState :: IsCrypto c => NodeState c DummySeal
emptyNodeState = NodeState
    { mempoolState = mempty
    , blockstoreState = emptyBlockchain
    , statestoreState = mempty
    }

nodeState
    :: [Tx c]
    -> Blockchain c (Tx c) s
    -> TxState c (Tx c)
    -> NodeState c s
nodeState mp bs st = NodeState { mempoolState = mp, blockstoreState = bs, statestoreState = st }

withNode
    :: ( IsCrypto c
       , Serialise s
       , Ord s
       )
    => s
    -- ^ The seal
    -> NodeState c s
    -> (NodeHandle c s -> IO a)
    -> IO a
withNode seal NodeState{..} k = do
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
    let dummyEvalBlock _ txs s = (map (\_ -> Right []) txs, s)
    ledger <- Ledger.newFromBlockStoreIO dummyEvalBlock blockStoreReader statestoreState
    runProtocol (\_ _ -> Right ()) blockScore metrics blkStore config $ \dispatchBlock ->
        Node.withNode
            cfg
            42
            mph
            ledger
            dispatchBlock
            (trivialConsensus seal)
            k

liftNode :: Node c a -> Session c DummySeal a
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
emptyBlockchain :: IsCrypto c => Blockchain c (Tx c) DummySeal
emptyBlockchain = fromGenesis $ sealBlock "" (emptyGenesisBlock epoch defaultBeneficiary)

-- | Run a 'Session' with the given initial node state
runSession :: IsCrypto c => NodeState c DummySeal -> Session c DummySeal () -> Assertion
runSession nst (Session sess) =
    withNode mempty nst $ \nh -> do
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
    -> Session c s ()
    -> m ()
runSessionWithState seal bindings (Session sess)= do
    let initialState = LegacyTxState $ Map.fromList bindings
    let genBlock = sealBlock seal $ emptyGenesisFromState epoch defaultBeneficiary initialState
    let nst = nodeState [] (fromGenesis genBlock) initialState
    liftIO $ withNode seal nst $ \nh -> do
        app <- API.app nh
        Wai.runSession (runReaderT sess nh) app

-- | Like 'runSessionWithState', but monomorphic in the seal.
runSessionWithState'
    :: ( IsCrypto c
       , MonadIO m
       )
    => [(ByteString, ByteString)]
    -> Session c DummySeal ()
    -> m ()
runSessionWithState' = runSessionWithState mempty

-- | Run a 'Session' with an empty node state. That is the node mempool
-- is empty and the blockchain has only the genesis block with an empty
-- dummy environment.
runEmptySession :: forall c m. (IsCrypto c, MonadIO m) => Session c DummySeal () -> m ()
runEmptySession = runSessionWithState' @c @m []


assertStatus :: HasCallStack => HTTP.Status -> Wai.SResponse -> Session c s ()
assertStatus want (Wai.simpleStatus -> have) = have @?= want

-- | Assert that the response can be deserialised to @API.Ok actual@
-- and @actual@ equals @expected@.
assertResultOK
    :: (HasCallStack, Serialise a, Eq a, Show a)
    => a -> Wai.SResponse -> Session c s ()
assertResultOK expected response = do
    result <- assertResponseBody response
    case result of
        API.Err err -> assertFailure $ "Received API error: " <> T.unpack err
        API.Ok v    -> expected @=? v

assertResponseBody
    :: (HasCallStack, Serialise a)
    => Wai.SResponse -> Session c s a
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
    -> Session c s Wai.SResponse
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


get :: Text -> Session c s Wai.SResponse
get path = request GET path (Nothing @())

post :: (Serialise a) => Text -> a -> Session c s Wai.SResponse
post path body = request POST path (Just body)
