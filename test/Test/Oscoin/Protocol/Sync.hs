module Test.Oscoin.Protocol.Sync (tests, props) where

import           Oscoin.Prelude

import           Oscoin.Crypto
import           Oscoin.Test.Crypto

import           Oscoin.Crypto.Blockchain (tip, (|>))
import           Oscoin.Crypto.Blockchain.Block
                 (Block, Sealed, emptyGenesisBlock, sealBlock)
import           Oscoin.Protocol.Sync as Sync
import qualified Oscoin.Protocol.Sync.Mock as Mock
import qualified Oscoin.Time as Time

import qualified Data.HashMap.Strict as HM
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
    ]

-- | For GHCi use.
props :: Dict (IsCrypto c) -> IO Bool
props _d = checkParallel $ Group "Test.Oscoin.Protocol.Sync"
    [("prop_withActivePeers_no_active_peers", prop_withActivePeers_no_active_peers)
    ,("prop_withActivePeers", prop_withActivePeers)
    ,("prop_getRemoteTip_sim_single", prop_getRemoteTip_sim_single)
    ,("prop_getRemoteTip_sim_two_peers_draw", prop_getRemoteTip_sim_two_peers_draw)
    ,("prop_getRemoteTip_sim_three_peers_majority", prop_getRemoteTip_sim_three_peers_majority)
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
    chain2 <- (flip (|>) chain1) <$> forAll (quickcheck (genBlockFrom (tip chain1)))

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
    chain2 <- (flip (|>) chain1) <$> forAll (quickcheck (genBlockFrom (tip chain1)))

    let worldState = Mock.emptyWorldState defaultGenesis
                   & over Mock.mockPeers (uncurry HM.insert testPeer1)
                   & over Mock.mockPeers (uncurry HM.insert (peer2, chain2))
                   & over Mock.mockPeers (uncurry HM.insert (peer3, chain1))
    let res = Mock.runMockSync worldState Mock.mockContext Sync.getRemoteTip

    -- Chain1 is picked, as both peer1 and peer3 are in agreement.

    res === (Right (tip chain1), [])

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
  where
