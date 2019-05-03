module Test.Oscoin.Protocol.Sync (tests, props) where

import           Oscoin.Prelude

import           Oscoin.Crypto
import           Oscoin.Test.Crypto

import           Oscoin.Crypto.Blockchain.Block
                 (Block, Sealed, emptyGenesisBlock, sealBlock)
import           Oscoin.Protocol.Sync as Sync
import qualified Oscoin.Protocol.Sync.Mock as Mock
import qualified Oscoin.Time as Time

import qualified Data.HashMap.Strict as HM
import           Lens.Micro (over)

import           Hedgehog
import           Hedgehog.Gen.QuickCheck (quickcheck)
import           Oscoin.Test.Crypto.Blockchain.Generators (genBlockchainFrom)
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitraryKeyPair)
import           Test.Oscoin.P2P.Gen (genNodeInfo)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: Dict (IsCrypto c) -> TestTree
tests _d = testGroup "Test.Oscoin.Protocol.Sync"
    [ testProperty "prop_withActivePeers_no_active_peers"  prop_withActivePeers_no_active_peers
    , testProperty "prop_withActivePeers" prop_withActivePeers
    ]

-- | For GHCi use.
props :: Dict (IsCrypto c) -> IO Bool
props _d = checkParallel $ Group "Test.Oscoin.Protocol.Sync"
    [("prop_withActivePeers_no_active_peers", prop_withActivePeers_no_active_peers)
    ,("prop_withActivePeers", prop_withActivePeers)
    ]

prop_withActivePeers_no_active_peers :: Property
prop_withActivePeers_no_active_peers = withTests 1 . property $ do
    let res = Mock.runMockSync Mock.emptyWorldState Mock.mockLayer (Sync.withActivePeers (const (pure ())))
    annotate "withActivePeers must fail with NoActivePeers" *> (res === Left NoActivePeers)

prop_withActivePeers :: Property
prop_withActivePeers = withTests 1 . property $ do
    testPeer <- forAll genActivePeer
    let worldState = Mock.emptyWorldState
                   & over Mock.mockPeers (uncurry HM.insert testPeer)
    let res = Mock.runMockSync worldState Mock.mockLayer (Sync.withActivePeers (const (pure ())))
    annotate "withActivePeers must succeed" *> (res === Right ())

genActivePeer :: Gen (Mock.MockPeer, Mock.PeerData)
genActivePeer = do
  (pk, _) <- quickcheck arbitraryKeyPair
  (,) <$> genNodeInfo pk <*> quickcheck (genBlockchainFrom defaultGenesis)
  where
      defaultGenesis :: Block MockCrypto [Word8] (Sealed MockCrypto Text)
      defaultGenesis = sealBlock mempty (emptyGenesisBlock Time.epoch)
