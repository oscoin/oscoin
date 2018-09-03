{-# LANGUAGE TupleSections #-}

module Oscoin.Test.P2P.Gossip.Membership (tests) where

import           Oscoin.Prelude

import           Oscoin.P2P.Gossip.Membership

import           Oscoin.Test.P2P.Gossip.Gen (Contacts, NodeId, SplitMixSeed)
import qualified Oscoin.Test.P2P.Gossip.Gen as Gen

import qualified Algebra.Graph.AdjacencyMap as Alga
import           Control.Monad ((>=>))
import qualified Data.HashSet as Set
import qualified Data.Map.Strict as Map
import           System.Random.SplitMix (seedSMGen')

import           Hedgehog
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

type Node  = Handle NodeId
type Nodes = Map NodeId Node

tests :: TestTree
tests = testGroup "Membership"
    [ testGroup "Overlay convergence with healthy underlay"
        [ testProperty "Disconnected network" . property $ do
            seed <- forAll Gen.splitMixSeed
            boot <- forAll $ Gen.disconnectedContacts Gen.defaultNetworkBounds
            propActiveDisconnected seed boot

        , testProperty "Circular network" . property $ do
            seed <- forAll Gen.splitMixSeed
            boot <- forAll $ Gen.circularContacts Gen.defaultNetworkBounds
            propActiveConnected seed boot

        , testProperty "Connected network" . property $ do
            seed <- forAll Gen.splitMixSeed
            boot <- forAll $ Gen.connectedContacts Gen.defaultNetworkBounds
            propActiveConnected seed boot
        ]
    ]

-- | Bootstrap the protocol with the respective contacts given by 'Contacts',
-- and assert the network of active views converged to a connected state.
propActiveConnected :: SplitMixSeed -> Contacts -> PropertyT IO ()
propActiveConnected seed boot = do
    peers <- lift $ runNetwork seed noopCallbacks boot
    annotateShow $ passiveNetwork peers
    assert $ isConnected (activeNetwork peers)

-- | Like 'propActiveConnected', but assert that the network converges to a
-- disconnected state.
--
-- This exists to suppress output which 'Test.Tasty.ExpectedFailure.expectFail'
-- would produce, and also because there's no point in letting hedgehog shrink
-- on failure.
propActiveDisconnected :: SplitMixSeed -> Contacts -> PropertyT IO ()
propActiveDisconnected seed boot = do
    peers <- lift $ runNetwork seed noopCallbacks boot
    annotateShow $ passiveNetwork peers
    assert $ not $ isConnected (activeNetwork peers)

--------------------------------------------------------------------------------

activeNetwork :: [(NodeId, Peers NodeId)] -> [(NodeId, [NodeId])]
activeNetwork = map (second (Set.toList . active))

passiveNetwork :: [(NodeId, Peers NodeId)] -> [(NodeId, [NodeId])]
passiveNetwork = map (second (Set.toList . passive))

isConnected :: [(NodeId, [NodeId])] -> Bool
isConnected adj =
    case Alga.dfsForest (Alga.fromAdjacencyList adj) of
        [_] -> True
        _   -> False

--------------------------------------------------------------------------------

runNetwork :: SplitMixSeed -> Callbacks NodeId -> Contacts -> IO [(NodeId, Peers NodeId)]
runNetwork seed cbs boot = do
    nodes <- initNodes seed cbs init'
    rpcs  <-
        flip foldMap (zip contacts (map snd nodes)) $ \(cs, hdl) ->
            runHyParView hdl $ joinAny cs
    settle (Map.fromList nodes) rpcs
    for nodes $ \(n, hdl) ->
        (n,) <$> runHyParView hdl getPeers'
  where
    (init', contacts) = unzip boot

settle :: Nodes -> [RPC NodeId] -> IO ()
settle nodes rpcs = loop $ pure rpcs
  where
    loop :: Maybe [RPC NodeId] -> IO ()
    loop = maybe (pure ()) (foldMap dispatch >=> loop)

    dispatch :: RPC NodeId -> IO (Maybe [RPC NodeId])
    dispatch rpc =
        for (Map.lookup (rpcRecipient rpc) nodes) $ \hdl ->
            runHyParView hdl (receive rpc)

initNodes :: SplitMixSeed -> Callbacks NodeId -> [NodeId] -> IO [(NodeId, Node)]
initNodes seed cbs ns = do
    let prng = seedSMGen' seed
    traverse (\n -> (n,) <$> new n defaultConfig prng cbs) ns

noopCallbacks :: Callbacks n
noopCallbacks = Callbacks
    { neighborUp   = const $ pure ()
    , neighborDown = const $ pure ()
    , connOpen     = const $ pure ()
    , connClose    = const $ pure ()
    }
