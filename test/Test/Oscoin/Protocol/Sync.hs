module Test.Oscoin.Protocol.Sync (tests, props) where

import           Oscoin.Prelude


import           Oscoin.Crypto
import           Oscoin.Test.Crypto

import qualified Oscoin.API.HTTP as API
import qualified Oscoin.API.HTTP.Internal as API
import           Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain
                 ( Blockchain
                 , blocks
                 , chainLength
                 , tip
                 , unsafeToBlockchain
                 , (|>)
                 )
import           Oscoin.Crypto.Blockchain.Block
                 (Block, Sealed, blockHash, blockHeader)
import           Oscoin.Data.Tx
import           Oscoin.P2P
                 ( Addr(..)
                 , mkAddr
                 , mkNodeId
                 , mkNodeInfo
                 , nodeHttpApiAddr
                 , readHost
                 )
import           Oscoin.Protocol (dispatchBlockSync, runProtocol)
import           Oscoin.Protocol.Sync as Sync
import qualified Oscoin.Protocol.Sync.Mock as Mock
import qualified Oscoin.Protocol.Sync.RealWorld as IO
import           Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (noLogger)
import           Oscoin.Telemetry.Metrics (labelsFromList, newMetricsStore)
import           Oscoin.Telemetry.Trace (noProbe)
import           Oscoin.Time.Chrono as Chrono

import           Codec.Serialise
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM
import           Data.Conduit
import           Data.Conduit.Combinators (sinkList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as Set
import           Data.IORef
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Maybe (fromJust)
import           Lens.Micro (over)
import           Network.Socket (PortNumber)
import           System.IO.Unsafe (unsafePerformIO)

import           Hedgehog
import           Hedgehog.Gen.QuickCheck (quickcheck)
import           Hedgehog.Internal.Property (forAllT)
import           Oscoin.Storage.Block.Memory (newBlockStoreIO)
import qualified Oscoin.Storage.Block.STM as STM
import           Oscoin.Test.Consensus.Nakamoto.Arbitrary ()
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Blockchain.Generators (genBlockchainFrom)
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitraryKeyPair)
import           Oscoin.Test.HTTP.Helpers (nodeState, withNode)
import           Oscoin.Test.Util (condensed, condensedS)
import           Test.Oscoin.P2P.Gen (genPortNumber)
import           Test.QuickCheck (Arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)


tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Protocol.Sync"
    [ testProperty "prop_withActivePeers_no_active_peers"  prop_withActivePeers_no_active_peers
    , testProperty "prop_withActivePeers" prop_withActivePeers
    , testProperty "prop_getRemoteTip_sim_single"  prop_getRemoteTip_sim_single
    , testProperty "prop_getRemoteTip_sim_two_peers_draw" prop_getRemoteTip_sim_two_peers_draw
    , testProperty "prop_getRemoteTip_sim_three_peers_majority" prop_getRemoteTip_sim_three_peers_majority
    , testProperty "prop_getRemoteTip_io_three_peers_majority" (prop_getRemoteTip_io_three_peers_majority d)
    , testProperty "prop_commonChainHeight_sim_full_agreement" prop_commonChainHeight_sim_full_agreement
    , testProperty "prop_commonChainHeight_io_full_agreement"  (prop_commonChainHeight_io_full_agreement d)
    , testProperty "prop_commonChainHeight_sim_partial_agreement" prop_commonChainHeight_sim_partial_agreement
    , testProperty "prop_commonChainHeight_sim_dishonest" prop_commonChainHeight_sim_dishonest
    , testProperty "prop_getBlocks_sim"  prop_getBlocks_sim
    , testProperty "prop_getBlockHeaders_sim"  prop_getBlockHeaders_sim
    , testProperty "prop_syncBlocks_sim_full" prop_syncBlocks_sim_full
    , testProperty "prop_syncBlocks_io_full" (prop_syncBlocks_io_full d)
    , testProperty "prop_syncBlocks_sim_missing" prop_syncBlocks_sim_missing
    , testProperty "prop_syncBlocks_io_missing" (prop_syncBlocks_io_missing d)
    , testProperty "prop_sync_io_mutual_consensus" (prop_sync_io_mutual_consensus d)
    ]

-- | For GHCi use.
props :: Dict (IsCrypto c) -> IO Bool
props d = checkParallel $ Group "Test.Oscoin.Protocol.Sync"
    [("prop_withActivePeers_no_active_peers", prop_withActivePeers_no_active_peers)
    ,("prop_withActivePeers", prop_withActivePeers)
    ,("prop_getRemoteTip_sim_single", prop_getRemoteTip_sim_single)
    ,("prop_getRemoteTip_sim_two_peers_draw", prop_getRemoteTip_sim_two_peers_draw)
    ,("prop_getRemoteTip_sim_three_peers_majority", prop_getRemoteTip_sim_three_peers_majority)
    ,("prop_getRemoteTip_io_three_peers_majority", prop_getRemoteTip_io_three_peers_majority d)
    ,("prop_commonChainHeight_sim_full_agreement", prop_commonChainHeight_sim_full_agreement)
    ,("prop_commonChainHeight_io_full_agreement",  prop_commonChainHeight_io_full_agreement d)
    ,("prop_commonChainHeight_sim_partial_agreement", prop_commonChainHeight_sim_partial_agreement)
    ,("prop_commonChainHeight_sim_dishonest", prop_commonChainHeight_sim_dishonest)
    ,("prop_getBlocks_sim", prop_getBlocks_sim)
    ,("prop_getBlockHeaders_sim", prop_getBlockHeaders_sim)
    ,("prop_syncBlocks_sim_full", prop_syncBlocks_sim_full)
    ,("prop_syncBlocks_io_full", prop_syncBlocks_io_full d)
    ,("prop_syncBlocks_sim_missing", prop_syncBlocks_sim_missing)
    ,("prop_syncBlocks_io_missing", prop_syncBlocks_io_missing d)
    ,("prop_sync_io_mutual_consensus", prop_sync_io_mutual_consensus d)
    ]

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

prop_withActivePeers_no_active_peers :: Property
prop_withActivePeers_no_active_peers = property $ do
    let res = Mock.runMockSync mockState
                               Mock.mockContext
                               (Sync.withActivePeers (const (pure ())))
    annotate "withActivePeers must fail with NoActivePeers" *> (res === (Left NoActivePeers, []))

prop_withActivePeers :: Property
prop_withActivePeers = property $ do
    testPeer <- forAll genMockPeer
    let worldState = mockState
                   & over Mock.mockPeers (uncurry HM.insert testPeer)
    let res = Mock.runMockSync worldState Mock.mockContext (Sync.withActivePeers (const (pure ())))
    annotate "withActivePeers must succeed" *> (res === (Right (), []))

prop_getRemoteTip_sim_single :: Property
prop_getRemoteTip_sim_single = property $ do
    testPeer@(_, peer1Chain) <- forAll genMockPeer
    let worldState = mockState
                   & over Mock.mockPeers (uncurry HM.insert testPeer)
    let res = Mock.runMockSync worldState Mock.mockContext Sync.getRemoteTip
    res === (Right (tip peer1Chain), [])

-- Test that in case there is no clear winner (by frequency) the highest
-- is picked.
prop_getRemoteTip_sim_two_peers_draw :: Property
prop_getRemoteTip_sim_two_peers_draw = property $ do
    testPeer1@(_, chain1) <- forAll genMockPeer
    (peer2, _) <- forAll genMockPeer
    chain2 <- flip (|>) chain1 <$> forAll (quickcheck (genBlockFrom (tip chain1)))

    let worldState = mockState
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain2))
    let res = Mock.runMockSync worldState Mock.mockContext Sync.getRemoteTip

    res === (Right (tip chain2), [])

prop_getRemoteTip_sim_three_peers_majority :: Property
prop_getRemoteTip_sim_three_peers_majority = property $ do
    testPeer1@(_, chain1) <- forAll genMockPeer
    (peer2, _) <- forAll genMockPeer
    (peer3, _) <- forAll genMockPeer
    chain2 <- flip (|>) chain1 <$> forAll (quickcheck (genBlockFrom (tip chain1)))

    let worldState = mockState
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain2))
                   & over Mock.mockPeers (uncurry HM.insert (peer3, chain1))
    let res = Mock.runMockSync worldState Mock.mockContext Sync.getRemoteTip

    -- Chain1 is picked, as both peer1 and peer3 are in agreement.

    res === (Right (tip chain1), [])

prop_getRemoteTip_io_three_peers_majority :: Dict (IsCrypto c) -> Property
prop_getRemoteTip_io_three_peers_majority d@Dict = withTests 1 . property $ do
    testPeer1@(peer1, chain1) <- forAllT (genNakamotoPeer d)
    (peer2, _) <- forAllT (genNakamotoPeer d)
    (peer3, _) <- forAllT (genNakamotoPeer d)
    chain2 <- flip (|>) chain1 <$> forAll (quickcheck (genBlockFrom (tip chain1)))

    let getActive = pure $ Set.fromList [peer1, peer2, peer3]

    withNodes d [testPeer1, (peer2, chain2), (peer3, chain1)] $ do
        res <- liftIO $ STM.withBlockStore chain1 Nakamoto.blockScore $ \(localChainReader,_) -> do
          syncContext <- IO.newSyncContext getActive localChainReader [] noProbe
          IO.runSync syncContext Sync.getRemoteTip

        -- Chain1 is picked, as both peer1 and peer3 are in agreement.
        res === tip chain1

testRetryPolicy :: RetryPolicy
testRetryPolicy = RetryPolicy linearBacktrackPolicy 0 20 0 5

-- Setup a test scenario with three nodes, where 2 has the same chain and the
-- third one half of it.
prop_commonChainHeight_sim_full_agreement :: Property
prop_commonChainHeight_sim_full_agreement = property $ do
    testPeer1@(_, chain1) <- forAll genMockPeer
    (peer2, _) <- forAll genMockPeer

    -- We build the common segment by cutting in half the chain of node1 &
    -- node2.
    let common = halfChain chain1

    -- We now add some extra blocks on the common segment, creating chain3.
    chain3 <- extendChain common

    let localTip   = height $ tip chain3
    let worldState = mockStateFrom chain3
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain1))
    let res = Mock.runMockSync worldState
                               Mock.mockContext
                               (Sync.commonChainHeightWith testRetryPolicy localTip)

    -- The height of the tip of the common chain is picked.
    res === (Right (height (tip common)), [])

prop_commonChainHeight_io_full_agreement :: Dict (IsCrypto c) -> Property
prop_commonChainHeight_io_full_agreement d@Dict = withTests 1 . property $ do
    testPeer1@(peer1, chain1) <- forAllT (genNakamotoPeer d)
    (peer2, _) <- forAllT (genNakamotoPeer d)

    let getActive = pure $ Set.fromList [peer1, peer2]
    let common = halfChain chain1

    chain3 <- foldl' (flip (|>)) common -- fuse the chains together
            . drop 1  -- drop 'tip common'
            . toOldestFirst
            . Chrono.reverse
            . blocks
           <$> forAllT (quickcheck (genBlockchainFrom (tip common)))

    let localTip   = height $ tip chain3

    withNodes d [testPeer1, (peer2, chain1)] $ do
        res <- liftIO $ STM.withBlockStore chain3 Nakamoto.blockScore $ \(localChainReader,_) -> do
          syncContext <- IO.newSyncContext getActive localChainReader [] noProbe
          IO.runSync syncContext (Sync.commonChainHeightWith testRetryPolicy localTip)

        res === height (tip common)

-- Setup a test scenario where the two remote peers are not in agreement. In
-- particular, node2 is on a fork.
prop_commonChainHeight_sim_partial_agreement :: Property
prop_commonChainHeight_sim_partial_agreement = property $ do
    testPeer1@(_, chain1) <- forAll genMockPeer
    (peer2, _) <- forAll genMockPeer

    -- We build the common segment by cutting in half the chain of node1
    let common = halfChain chain1

    -- We now add some extra blocks on the common segment, creating chain3.
    chain2 <- extendChain common
    chain3 <- extendChain common

    let localTip   = height $ tip chain3
    let worldState = mockStateFrom chain3
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain2))
    let res = Mock.runMockSync worldState
                               Mock.mockContext
                               (Sync.commonChainHeightWith testRetryPolicy localTip)

    -- The height of the tip of the common chain is picked.
    res === (Right (height (tip common)), [])

-- Setup a test scenario where the two remote peers are not in agreement, and
-- node2 is dishonest, so it's generating a completely different chain and
-- tricking node3 to follow it.
prop_commonChainHeight_sim_dishonest :: Property
prop_commonChainHeight_sim_dishonest = property $ do
    testPeer1@(_, chain1) <- forAll genMockPeer
    (peer2, chain2) <- forAll genMockPeer

    -- We build the common segment by cutting in half the chain of node1
    let common = halfChain chain1

    -- We now add some extra blocks on the common segment, creating chain3.
    chain3 <- extendChain common

    let localTip   = height $ tip chain3
    let worldState = mockStateFrom chain3
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain2))
    let res = Mock.runMockSync worldState
                               Mock.mockContext
                               (Sync.commonChainHeightWith testRetryPolicy localTip)

    annotate (condensedS chain1)
    annotate (condensedS chain2)
    annotate (condensedS chain3)

    -- The sync should start back from 0, as we cannot find a shared height.
    res === (Right 0, [])

-- This property tests the data fetcher of the simulator, as a preliminary
-- step for 'prop_syncBlocks_sim_full'.
prop_getBlocks_sim :: Property
prop_getBlocks_sim = property $ do
    testPeer1@(_, chain1) <- forAll genMockPeer
    let allBlocksNoGenesis = drop 1 $ Oscoin.Prelude.reverse (toNewestFirst $ blocks chain1)

    let worldState = mockState
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)

    let fullRange = fromJust $ range mockGenesis (tip chain1)

    let fetchSubset = do
          fetcher <- asks scDataFetcher
          withActivePeer $ \activePeer -> do
              res <- lift (fetch fetcher activePeer SGetBlocks fullRange)
              case res of
                Left err -> throwError $ AllPeersSyncError [err]
                Right x  -> OldestFirst <$> lift (runConduit (x .| sinkList))

    let actual = Mock.runMockSync worldState Mock.mockContext fetchSubset
    let expected = OldestFirst allBlocksNoGenesis

    annotate (show fullRange) >> actual === (Right expected, [])

prop_getBlockHeaders_sim :: Property
prop_getBlockHeaders_sim = property $ do
    testPeer1@(_, chain1) <- forAll genMockPeer
    let allBlockHeadersNoGenesis =
              map blockHeader
            . drop 1
            . Oscoin.Prelude.reverse
            . toNewestFirst
            . blocks
            $ chain1

    let worldState = mockState
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)

    let fullRange = fromJust $ range mockGenesis (tip chain1)

    let fetchSubset = do
          fetcher <- asks scDataFetcher
          withActivePeer $ \activePeer -> do
              res <- lift (fetch fetcher activePeer SGetBlockHeaders fullRange)
              case res of
                Left err -> throwError $ AllPeersSyncError [err]
                Right x  -> lift $ runConduit (x .| sinkList)

    let actual   = Mock.runMockSync worldState Mock.mockContext fetchSubset
    let expected = allBlockHeadersNoGenesis

    actual === (Right expected, [])

-- | This property tests the 'syncBlocks' function using the simulated
-- environment.
prop_syncBlocks_sim_full :: Property
prop_syncBlocks_sim_full = property $ do
    testPeer1@(_, chain1) <- forAll genMockPeer
    (peer2, _) <- forAll genMockPeer
    (peer3, _) <- forAll genMockPeer
    let allBlocksNoGenesis = drop 1 $ Oscoin.Prelude.reverse (toNewestFirst $ blocks chain1)

    let worldState = mockState
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain1))
                   & over Mock.mockPeers (uncurry HM.insert (peer3, chain1))

    let res = Mock.runMockSync worldState Mock.mockContext (Sync.syncBlocks 0 (tip chain1))

    res === (Right (), map SyncBlock allBlocksNoGenesis)

prop_syncBlocks_io_full :: Dict (IsCrypto c) -> Property
prop_syncBlocks_io_full d@Dict = withTests 1 . property $ do
    testPeer1@(peer1, chain1) <- forAllT (genNakamotoPeer d)
    (peer2, _) <- forAllT (genNakamotoPeer d)
    (peer3, _) <- forAllT (genNakamotoPeer d)

    let allBlocksNoGenesis = drop 1 $ Oscoin.Prelude.reverse (toNewestFirst $ blocks chain1)
    let getActive = pure $ Set.fromList [peer1, peer2, peer3]

    evts <- liftIO newTQueueIO

    withNodes d [testPeer1, (peer2, chain1), (peer3, chain1)] $ do
        liftIO $ STM.withBlockStore chain1 Nakamoto.blockScore $ \(localChainReader,_) -> do
          syncContext <- IO.newSyncContext getActive localChainReader [atomically . writeTQueue evts] noProbe
          IO.runSync syncContext (Sync.syncBlocks 0 (tip chain1))

        allEvts <- liftIO $ atomically (flushTQueue evts)
        allEvts === map SyncBlock allBlocksNoGenesis

-- We setup 3 peers, where the 2nd peer is asked to fetch some blocks in the
-- middle but he has a stale view of the chain. Nevertheless, syncBlocks
-- should be able to download the missing blocks.
prop_syncBlocks_sim_missing :: Property
prop_syncBlocks_sim_missing = property $ do
    (peer1, chain1) <- forAll genMockPeer
    (peer2, _) <- forAll genMockPeer
    (peer3, _) <- forAll genMockPeer

    let worldState = mockState
                   & over Mock.mockPeers (uncurry HM.insert (peer1, chain1))
                   & over Mock.mockPeers (uncurry HM.insert (peer2, unsafeToBlockchain [mockGenesis]))
                   & over Mock.mockPeers (uncurry HM.insert (peer3, chain1))

    let allBlocksNoGenesis =
            drop 1 $ Oscoin.Prelude.reverse (toNewestFirst $ blocks chain1)

    let res = Mock.runMockSync worldState Mock.mockContext (Sync.syncBlocks 0 (tip chain1))

    annotate (condensedS chain1)

    -- The event might arrive in any order, so we need to sort in order to
    -- compare.
    second sort res === (Right (), sort $ map SyncBlock allBlocksNoGenesis)

prop_syncBlocks_io_missing :: Dict (IsCrypto c) -> Property
prop_syncBlocks_io_missing d@Dict = withTests 1 . property $ do
    testPeer1@(peer1, chain1) <- forAllT (genNakamotoPeer d)
    (peer2, _) <- forAllT (genNakamotoPeer d)
    (peer3, _) <- forAllT (genNakamotoPeer d)

    let allBlocksNoGenesis =
            drop 1 $ Oscoin.Prelude.reverse (toNewestFirst $ blocks chain1)

    let getActive = pure $ Set.fromList [peer1, peer2, peer3]

    withNodes d [testPeer1, (peer2, unsafeToBlockchain [nakamotoGenesis d]), (peer3, chain1)] $ do
        evts <- liftIO newTQueueIO

        liftIO $ STM.withBlockStore chain1 Nakamoto.blockScore $ \(localChainReader,_) -> do
          syncContext <- IO.newSyncContext getActive localChainReader [atomically . writeTQueue evts] noProbe
          IO.runSync syncContext (Sync.syncBlocks 0 (tip chain1))

        allEvts <- liftIO $ atomically (flushTQueue evts)

        -- The event might arrive in any order, so we need to sort in order to
        -- compare.
        sort allEvts === sort (map SyncBlock allBlocksNoGenesis)

-- | Simulates two nodes talking to each other, where one would be on the
-- correct chain and the other one lagging behind. We want to assess that they
-- eventually end up with the same chain.
prop_sync_io_mutual_consensus :: forall c. Dict (IsCrypto c) -> Property
prop_sync_io_mutual_consensus d@Dict = withTests 1 . property $ do

    testPeer1@(peer1, chain1) <- forAllT (genNakamotoPeer d)
    (peer2, _)                <- forAllT (genNakamotoPeer d)

    -- The second peer starts from a cold-sync situation.
    let chain2 = unsafeToBlockchain [nakamotoGenesis d]

    let noGenesis = OldestFirst . drop 1 . Oscoin.Prelude.reverse . toNewestFirst . blocks
    let handleEvt proto = \case
          SyncBlock b -> dispatchBlockSync proto b
          _ -> pure ()

    -- Starts both peer1 and peer2 as remote nodes.
    (blks1, blks2) <- withNodes d [testPeer1, (peer2, chain2)] $ do
        -- Starts both peer1 and peer2 as local nodes.
        let doSync chain peers = liftIO $ do
              metricsStore <- newMetricsStore $ labelsFromList []
              store@(public, private) <- newBlockStoreIO (NewestFirst $ nakamotoGenesis d :| [])
              liftIO $ Abstract.insertBlocksNaive private (noGenesis chain)
              runProtocol (\_ _ -> Right ())
                          Nakamoto.blockScore
                          (Telemetry.newTelemetryStore noLogger metricsStore)
                          store
                          Consensus.testConfig $ \proto ->
                  liftIO $ do
                      ctx <- IO.newSyncContext (pure $ Set.fromList peers) public [handleEvt proto] noProbe
                      IO.runSync ctx (replicateM_ 1 (Sync.syncUntil (\_ _ _ -> False)))
                      Abstract.getBlocksByParentHash public (blockHash $ nakamotoGenesis d)

        liftIO $ Async.concurrently (doSync chain1 [peer2])
                                    (doSync chain2 [peer1])

    annotate ("Initial chain 1: " <> toS (condensed $ noGenesis chain1))
    annotate ("Initial chain 2: " <> toS (condensed $ noGenesis chain2))
    annotate ("Final chain 1: "   <> toS (condensed $ Chrono.reverse blks1))
    annotate ("Final chain 2: "   <> toS (condensed $ Chrono.reverse blks2))
    blks1 === blks2

{------------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

-- | Chops the input 'Blockchain' and return half of it.
halfChain :: Blockchain c tx s -> Blockchain c tx s
halfChain chain = unsafeToBlockchain
                . Oscoin.Prelude.reverse
                . take (round $ fromIntegral @Int @Double (chainLength chain) / 2.0)
                . toOldestFirst
                . Chrono.reverse
                . blocks
                $ chain

-- | Extends the input chain (at the tip) with a bunch of random blocks.
extendChain
    :: ( IsCrypto c
       , Monad m
       , Arbitrary tx
       , Arbitrary s
       , Serialise tx
       , Serialise s
       , Show tx
       , Show s
       )
    => Blockchain c tx s
    -> PropertyT m (Blockchain c tx s)
extendChain common =
        foldl' (flip (|>)) common -- fuse the chains together
     .  drop 1  -- drop 'tip common'
     .  toOldestFirst
     .  Chrono.reverse
     .  blocks
    <$> forAll (quickcheck (genBlockchainFrom (tip common)))

-- | When given a list of active peers and their initial chains, it spins up
-- the revelant HTTP APIs, execute the input action and teardown all the
-- nodes at the end.
withNodes
    :: (MonadIO m, MonadCatch m)
    => Dict (IsCrypto c)
    -> [(ActivePeer c, Blockchain c (Tx c) Nakamoto.PoW)]
    -> m a
    -> m a
withNodes d@Dict peers action = do
    tokens <- liftIO $ newTBQueueIO (fromIntegral $ length peers)
    asyncs <- liftIO $ forM peers (spinUpNode d tokens)

    -- Wait for all the peers to be ready
    forM_ [1 .. length peers] $ \_ -> liftIO (atomically $ readTBQueue tokens)

    -- Run the action and tear down things in case of exceptions.
    res <- try action
    case res of
      Left (e :: SomeException) -> forM_ asyncs (liftIO . cancel) >> throwM e
      Right r                   -> forM_ asyncs (liftIO . cancel) >> pure r

-- | Spins up a node (in a green thread) listen on the HTTP API, with the
-- supplied initial chain.
spinUpNode
    :: Dict (IsCrypto c)
    -> TBQueue ()
    -> (ActivePeer c, Blockchain c (Tx c) Nakamoto.PoW)
    -> IO (Async ())
spinUpNode Dict tokens (peer, peerChain) = (\a -> link a >> pure a) =<<
    async (do
        let initialState = nodeState mempty peerChain emptyState
        withNode evaluateBlock validateTx Nakamoto.emptyPoW initialState $ \hdl ->
            API.runApi' noLogger
                        (atomically $ writeTBQueue tokens ())
                        (API.api identity)
                        (fromIntegral $ addrPort $ nodeHttpApiAddr peer)
                        hdl
    )

{------------------------------------------------------------------------------
  Constant values
------------------------------------------------------------------------------}

mockState :: Mock.WorldState
mockState = Mock.emptyWorldState mockGenesis

mockStateFrom
    :: Blockchain MockCrypto Mock.MockTx Mock.MockSeal
    -> Mock.WorldState
mockStateFrom = Mock.worldStateFrom

defaultGenesis
    :: Serialise s
    => Dict (IsCrypto c)
    -> s
    -> Block c tx (Sealed c s)
defaultGenesis Dict seal = someGenesisBlock seal

nakamotoGenesis :: Dict (IsCrypto c) -> Block c tx (Sealed c Nakamoto.PoW)
nakamotoGenesis d = defaultGenesis d Nakamoto.emptyPoW

mockGenesis :: Block MockCrypto Mock.MockTx (Sealed MockCrypto Mock.MockSeal)
mockGenesis = defaultGenesis (Dict :: Dict (IsCrypto MockCrypto)) mempty

{------------------------------------------------------------------------------
  Generators
------------------------------------------------------------------------------}

-- Generating unique ports
--
-- In order to test the Sync logic for the 'RealWorld' implementation, we need
-- to spin up some threads to actually bind on some ports in order to respond
-- to HTTP requests from other peers. However, generating such random ports is
-- not useful for these kind of tests, as ports on a local machine are a finite
-- resource and generating them randomly means it's more likely to get a
-- collision. If that happens, an exception would be raised and the tests would
-- fail. To avoid that, we simply keep track of a global HTTP port \"dispenser\",
-- which allocates ports sequentally and thus allows tests (even if ran in
-- parallel) to all succeed.

-- | A global counter for the HTTP ports.
--
-- N.B. We start from 0 for a simple reason: in case we reach the maximum
-- number of ports (i.e. maxBound :: Word16, i.e. 65535), the counter would
-- overflow, starting from 0. However, we cannot bind to 0 (nor to a bunch of
-- system-used ports). This is why in `nextUnusedHttpPort' we increment the
-- counter by 1 but we return /port + 3000/: This ensure that in case of
-- overflow we would resume back from an allowed range.
uniqueHttpPortRef :: IORef PortNumber
uniqueHttpPortRef = unsafePerformIO $ newIORef 0
{-# NOINLINE uniqueHttpPortRef #-}

-- N.b. the call to 'max' in the second element of the tuple ensures that when
-- we are approaching overflow, we would resume back from 3000.
nextUnusedHttpPort :: IO PortNumber
nextUnusedHttpPort =
    atomicModifyIORef' uniqueHttpPortRef
                       (\port -> (port + 1, max 3000 (port + 3000)))

genMockPeer :: Gen (Mock.MockPeer, Mock.PeerData)
genMockPeer = do
    let proof = Dict :: Dict (IsCrypto MockCrypto)
    port <- genPortNumber
    genActivePeer proof (defaultGenesis proof mempty) port

genNakamotoPeer
    :: forall c m. MonadIO m
    => Dict (IsCrypto c)
    -> GenT m (ActivePeer c, Blockchain c (Tx c) Nakamoto.PoW)
genNakamotoPeer d@Dict = do
    port <- liftIO nextUnusedHttpPort
    genActivePeer d (nakamotoGenesis d) port

genActivePeer
    :: forall c tx s m.
       ( Arbitrary tx
       , Serialise tx
       , Arbitrary s
       , Serialise s
       , Monad m
       )
    => Dict (IsCrypto c)
    -> Block c tx (Sealed c s)
    -- ^ The genesis block.
    -> PortNumber
    -> GenT m (ActivePeer c, Blockchain c tx s)
genActivePeer Dict genesis port = do
  (pk, _) <- quickcheck arbitraryKeyPair
  let localHost = case readHost "127.0.0.1" of
                    Left e  -> panic (show e)
                    Right l -> l
  (,) <$> pure (mkNodeInfo (mkAddr localHost port) (mkNodeId pk))
      <*> quickcheck (genBlockchainFrom genesis)
