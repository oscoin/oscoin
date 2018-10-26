module Network.Gossip.Test.Broadcast (tests, props) where

import           Prelude

import           Network.Gossip.Plumtree

import           Network.Gossip.Test.Assert (allEqual)
import           Network.Gossip.Test.Gen (Contacts, NodeId)
import qualified Network.Gossip.Test.Gen as Gen

import           Control.Applicative (liftA2)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async, pollSTM)
import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TQueue
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.Foldable (for_, traverse_)
import qualified Data.HashSet as Set
import           Data.IORef
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Traversable (for)

import           Hedgehog hiding (eval)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog

{-# ANN module ("HLint: ignore Use map" :: String) #-}

-- Node Storage ----------------------------------------------------------------

type Store = IORef (Map MessageId ByteString)

storeInsert :: Store -> MessageId -> ByteString -> IO ApplyResult
storeInsert ref mid payload = atomicModifyIORef' ref $ \m ->
    case Map.insertLookupWithKey (\_ v _ -> v) mid payload m of
        (Nothing, m') -> (m', Applied Nothing)
        (Just  _, _ ) -> (m , Stale   Nothing)

storeLookup :: Store -> MessageId -> IO (Maybe ByteString)
storeLookup ref mid = Map.lookup mid <$> readIORef ref

-- Global scheduler ------------------------------------------------------------

type Scheduler = TQueue (Async ())

schedulerDone :: Scheduler -> STM Bool
schedulerDone = go
  where
    go q = do
        task <- tryReadTQueue q
        case task of
            Nothing -> pure True
            Just  t -> do
                status <- pollSTM t
                case status of
                    Nothing         -> unGetTQueue q t *> go q
                    Just (Right ()) -> go q
                    Just (Left e)   -> error $ "Scheduled action threw: " <> show e

--------------------------------------------------------------------------------

data Network = Network
    { netNodes     :: Map NodeId Node
    , netScheduler :: Scheduler
    }

data Node = Node
    { nodeHandle :: Handle NodeId
    , nodeStore  :: Store
    }

tests :: TestTree
tests = testGroup "Broadcast"
    [ testGroup "Static Network"
        [ testProperty "Atomic Broadcast" propAtomicConnected ]
    ]

props :: IO Bool
props = checkParallel $ Group "Gossip.Broadcast"
    [ ("prop_atomic_connected", propAtomicConnected) ]

propAtomicConnected :: Property
propAtomicConnected = property $ do
    boot   <- forAll $ Gen.connectedContacts Gen.defaultNetworkBounds
    bcasts <- forAll $ genBroadcasts boot
    atomicBroadcast boot bcasts

-- | Assert atomicity of broadcasts.
--
-- A broadcast is said to be atomic if it (eventually) reaches all nodes.
atomicBroadcast
    :: Contacts
    -> [(Int, (MessageId, ByteString))]
    -> PropertyT IO ()
atomicBroadcast contacts bcasts = do
    stores  <- liftIO $ do
        network <- initNetwork contacts
        traverse_ (bcast network) bcasts

        atomically $
            schedulerDone (netScheduler network) >>= bool retry (pure ())

        Map.toList <$> traverse (readIORef . nodeStore) (netNodes network)

    annotateShow stores

    let stores' = nonEmpty $ map snd stores
    let bcasts' = map snd bcasts
    Just True === fmap allEqual stores'
    fmap NonEmpty.head stores' === Just (Map.fromList bcasts')

bcast :: Network -> (Int, (MessageId, ByteString)) -> IO ()
bcast network (root, msg) = do
    let node = snd $ Map.elemAt root (netNodes network)
    void $ uncurry (storeInsert (nodeStore node)) msg
    runBroadcast network node $ uncurry broadcast msg

--------------------------------------------------------------------------------

genBroadcasts :: MonadGen m => Contacts -> m [(Int, (MessageId, ByteString))]
genBroadcasts (length -> glen) = do
    roots  <- Gen.nonEmpty (Range.linear 1 glen) (Gen.element [0..(glen - 1)])
    bcasts <-
        Gen.list (Range.singleton (length roots)) $
            liftA2 (,) (Gen.prune $ Gen.utf8 (Range.singleton 8) Gen.alphaNum)
                       (Gen.prune $ Gen.bytes (Range.singleton 32))

    pure $ zip (NonEmpty.toList roots) bcasts

--------------------------------------------------------------------------------

initNetwork :: Contacts -> IO Network
initNetwork contacts = do
    nodes <-
        for contacts $ \(self, peers) -> do
            store <- newIORef mempty
            hdl   <- new self
            pure (self, peers, Node hdl store)

    sched <- newTQueueIO
    let network = Network
                { netNodes     = Map.fromList $ map (\(self, _, node) -> (self, node)) nodes
                , netScheduler = sched
                }

    for_ nodes $ \(_, peers, node) ->
        runBroadcast network node $ resetPeers (Set.fromList peers)

    pure network

runBroadcast :: Network -> Node -> Plumtree NodeId a -> IO a
runBroadcast network node ma = runPlumtree (nodeHandle node) ma >>= eval
  where
    eval = \case
        ApplyMessage mid msg k ->
            storeInsert (nodeStore node) mid msg >>= k >>= eval

        LookupMessage mid k ->
            storeLookup (nodeStore node) mid >>= k >>= eval

        SendEager to msg k -> do
            onNode network to $ receive msg
            k >>= eval

        SendLazy to ihave k -> do
            task <-
                async $ do
                    threadDelay 30000
                    onNode network to $ receive (IHaveM ihave)
            atomically $ writeTQueue (netScheduler network) task
            k >>= eval

        Later _ _ action k -> do
            task <-
                async $ do
                    threadDelay 60000
                    runBroadcast network node action
            atomically $ writeTQueue (netScheduler network) task
            k >>= eval

        Cancel _ k -> k >>= eval

        Done a -> pure a

onNode :: Network -> NodeId -> Plumtree NodeId a -> IO ()
onNode network n ma =
    for_ (Map.lookup n (netNodes network)) $ \node ->
        runBroadcast network node ma
