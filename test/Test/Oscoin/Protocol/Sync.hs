module Test.Oscoin.Protocol.Sync (tests, props) where

import           Oscoin.Prelude


import           Oscoin.Crypto
import           Oscoin.Test.Crypto

import           Oscoin.Crypto.Blockchain
                 (blocks, tip, unsafeToBlockchain, (|>))
import           Oscoin.Crypto.Blockchain.Block
                 (Block, Sealed, blockHeader, emptyGenesisBlock, sealBlock)
import           Oscoin.Protocol.Sync as Sync
import qualified Oscoin.Protocol.Sync.Mock as Mock
import qualified Oscoin.Time as Time
import           Oscoin.Time.Chrono

import           Data.Conduit
import           Data.Conduit.Combinators (sinkList)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import           Lens.Micro (over)

import           Hedgehog
import           Hedgehog.Gen.QuickCheck (quickcheck)
import           Oscoin.Test.Crypto.Blockchain.Block.Generators (genBlockFrom)
import           Oscoin.Test.Crypto.Blockchain.Block.Helpers
                 (defaultBeneficiary)
import           Oscoin.Test.Crypto.Blockchain.Generators (genBlockchainFrom)
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitraryKeyPair)
import           Test.Oscoin.P2P.Gen (genNodeInfo)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)


tests :: Dict (IsCrypto c) -> TestTree
tests _d = testGroup "Test.Oscoin.Protocol.Sync"
    [ testProperty "prop_withActivePeers_no_active_peers"  prop_withActivePeers_no_active_peers
    , testProperty "prop_withActivePeers" prop_withActivePeers
    , testProperty "prop_getRemoteTip_sim_single"  prop_getRemoteTip_sim_single
    , testProperty "prop_getRemoteTip_sim_two_peers_draw" prop_getRemoteTip_sim_two_peers_draw
    , testProperty "prop_getRemoteTip_sim_three_peers_majority" prop_getRemoteTip_sim_three_peers_majority
    , testProperty "prop_getBlocks_sim"  prop_getBlocks_sim
    , testProperty "prop_getBlockHeaders_sim"  prop_getBlockHeaders_sim
    , testProperty "prop_syncBlocks_sim_full" prop_syncBlocks_sim_full
    , testProperty "prop_syncBlocks_sim_missing" prop_syncBlocks_sim_missing
    ]

-- | For GHCi use.
props :: Dict (IsCrypto c) -> IO Bool
props _d = checkParallel $ Group "Test.Oscoin.Protocol.Sync"
    [("prop_withActivePeers_no_active_peers", prop_withActivePeers_no_active_peers)
    ,("prop_withActivePeers", prop_withActivePeers)
    ,("prop_getRemoteTip_sim_single", prop_getRemoteTip_sim_single)
    ,("prop_getRemoteTip_sim_two_peers_draw", prop_getRemoteTip_sim_two_peers_draw)
    ,("prop_getRemoteTip_sim_three_peers_majority", prop_getRemoteTip_sim_three_peers_majority)
    ,("prop_getBlocks_sim", prop_getBlocks_sim)
    ,("prop_getBlockHeaders_sim", prop_getBlockHeaders_sim)
    ,("prop_syncBlocks_sim_full", prop_syncBlocks_sim_full)
    ,("prop_syncBlocks_sim_missing", prop_syncBlocks_sim_missing)
    ]

prop_withActivePeers_no_active_peers :: Property
prop_withActivePeers_no_active_peers = property $ do
    let res = Mock.runMockSync (Mock.emptyWorldState defaultGenesis)
                               Mock.mockContext
                               (Sync.withActivePeers (const (pure ())))
    annotate "withActivePeers must fail with NoActivePeers" *> (res === (Left NoActivePeers, []))

prop_withActivePeers :: Property
prop_withActivePeers = property $ do
    testPeer <- forAll genActivePeer
    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert testPeer)
    let res = Mock.runMockSync worldState Mock.mockContext (Sync.withActivePeers (const (pure ())))
    annotate "withActivePeers must succeed" *> (res === (Right (), []))

prop_getRemoteTip_sim_single :: Property
prop_getRemoteTip_sim_single = property $ do
    testPeer@(_, peer1Chain) <- forAll genActivePeer
    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert testPeer)
    let res = Mock.runMockSync worldState Mock.mockContext Sync.getRemoteTip
    res === (Right (tip peer1Chain), [])

-- Test that in case there is no clear winner (by frequency) the highest
-- is picked.
prop_getRemoteTip_sim_two_peers_draw :: Property
prop_getRemoteTip_sim_two_peers_draw = property $ do
    testPeer1@(_, chain1) <- forAll genActivePeer
    (peer2, _) <- forAll genActivePeer
    chain2 <- flip (|>) chain1 <$> forAll (quickcheck (genBlockFrom (tip chain1)))

    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain2))
    let res = Mock.runMockSync worldState Mock.mockContext Sync.getRemoteTip

    res === (Right (tip chain2), [])

prop_getRemoteTip_sim_three_peers_majority :: Property
prop_getRemoteTip_sim_three_peers_majority = property $ do
    testPeer1@(_, chain1) <- forAll genActivePeer
    (peer2, _) <- forAll genActivePeer
    (peer3, _) <- forAll genActivePeer
    chain2 <- flip (|>) chain1 <$> forAll (quickcheck (genBlockFrom (tip chain1)))

    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain2))
                   & over Mock.mockPeers (uncurry HM.insert (peer3, chain1))
    let res = Mock.runMockSync worldState Mock.mockContext Sync.getRemoteTip

    -- Chain1 is picked, as both peer1 and peer3 are in agreement.

    res === (Right (tip chain1), [])

-- This property tests the data fetcher of the simulator, as a preliminary
-- step for 'prop_syncBlocks_sim_full'.
prop_getBlocks_sim :: Property
prop_getBlocks_sim = property $ do
    testPeer1@(_, chain1) <- forAll genActivePeer
    let allBlocksNoGenesis = drop 1 $ Oscoin.Prelude.reverse (toNewestFirst $ blocks chain1)

    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)

    let fullRange = fromJust $ range defaultGenesis (tip chain1)

    let fetchSubset = do
          fetcher <- asks scDataFetcher
          withActivePeer $ \activePeer -> do
              res <- lift (fetch fetcher activePeer SGetBlocks fullRange)
              case res of
                Left _  -> throwError AllPeersTimeoutError
                Right x -> OldestFirst <$> lift (runConduit (x .| sinkList))

    let actual = Mock.runMockSync worldState Mock.mockContext fetchSubset
    let expected = OldestFirst allBlocksNoGenesis

    annotate (show fullRange) >> actual === (Right expected, [])

prop_getBlockHeaders_sim :: Property
prop_getBlockHeaders_sim = property $ do
    testPeer1@(_, chain1) <- forAll genActivePeer
    let allBlockHeadersNoGenesis =
              map blockHeader
            . drop 1
            . Oscoin.Prelude.reverse
            . toNewestFirst
            . blocks
            $ chain1

    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)

    let fullRange = fromJust $ range defaultGenesis (tip chain1)

    let fetchSubset = do
          fetcher <- asks scDataFetcher
          withActivePeer $ \activePeer -> do
              res <- lift (fetch fetcher activePeer SGetBlockHeaders fullRange)
              case res of
                Left _  -> throwError AllPeersTimeoutError
                Right x -> pure x

    let actual = Mock.runMockSync worldState Mock.mockContext fetchSubset
    let expected = OldestFirst allBlockHeadersNoGenesis

    actual === (Right expected, [])

-- | This property tests the 'syncBlocks' function using the simulated
-- environment.
prop_syncBlocks_sim_full :: Property
prop_syncBlocks_sim_full = property $ do
    testPeer1@(_, chain1) <- forAll genActivePeer
    (peer2, _) <- forAll genActivePeer
    (peer3, _) <- forAll genActivePeer
    let allBlocksNoGenesis = drop 1 $ Oscoin.Prelude.reverse (toNewestFirst $ blocks chain1)

    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain1))
                   & over Mock.mockPeers (uncurry HM.insert (peer3, chain1))

    let fullRange = fromJust $ range defaultGenesis (tip chain1)

    let res = Mock.runMockSync worldState Mock.mockContext (Sync.syncBlocks fullRange)

    res === (Right (), map SyncBlock allBlocksNoGenesis)

-- We setup 3 peers, where the 2nd peer is asked to fetch some blocks in the
-- middle but he has a stale view of the chain. Nevertheless, syncBlocks
-- should be able to download the missing blocks.
prop_syncBlocks_sim_missing :: Property
prop_syncBlocks_sim_missing = property $ do
    (peer1, chain1) <- forAll genActivePeer
    (peer2, _) <- forAll genActivePeer
    (peer3, _) <- forAll genActivePeer

    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert (peer1, chain1))
                   & over Mock.mockPeers (uncurry HM.insert (peer2, unsafeToBlockchain [defaultGenesis]))
                   & over Mock.mockPeers (uncurry HM.insert (peer3, chain1))

    let allBlocksNoGenesis =
            drop 1 $ Oscoin.Prelude.reverse (toNewestFirst $ blocks chain1)

    let fullRange = fromJust $ range defaultGenesis (tip chain1)

    let res = Mock.runMockSync worldState Mock.mockContext (Sync.syncBlocks fullRange)

    -- The event might arrive in any order, so we need to sort in order to
    -- compare.
    second sort res === (Right (), sort $ map SyncBlock allBlocksNoGenesis)

{------------------------------------------------------------------------------
  Constant values
------------------------------------------------------------------------------}

defaultGenesis :: Block MockCrypto Mock.MockTx (Sealed MockCrypto Mock.MockSeal)
defaultGenesis = sealBlock mempty (emptyGenesisBlock Time.epoch defaultBeneficiary)

{------------------------------------------------------------------------------
  Generators
------------------------------------------------------------------------------}

genActivePeer :: Gen (Mock.MockPeer, Mock.PeerData)
genActivePeer = do
  (pk, _) <- quickcheck arbitraryKeyPair
  (,) <$> genNodeInfo pk <*> quickcheck (genBlockchainFrom defaultGenesis)
