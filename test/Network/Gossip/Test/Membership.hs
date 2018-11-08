{-# LANGUAGE TupleSections #-}

module Network.Gossip.Test.Membership (tests, props) where

import           Prelude

import           Network.Gossip.HyParView

import           Network.Gossip.Test.Gen (Contacts, NodeId, SplitMixSeed)
import qualified Network.Gossip.Test.Gen as Gen

import qualified Algebra.Graph.AdjacencyMap as Alga
import           Control.Monad.Trans.Class (lift)
import           Data.Bifunctor (second)
import           Data.Foldable (for_)
import qualified Data.HashSet as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Traversable (for)
import           System.Random.SplitMix (seedSMGen')

import           Hedgehog hiding (eval)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

{-# ANN module ("HLint: ignore Use map" :: String) #-}

type Node  = Handle NodeId
type Nodes = Map NodeId Node

tests :: TestTree
tests = testGroup "Membership"
    [ testGroup "Overlay convergence with healthy underlay"
        [ testProperty "Disconnected network" propDisconnected
        , testProperty "Circular network"     propCircularConnected
        , testProperty "Connected network"    propConnected
        ]
    ]

-- | Gor GHCi.use.
props :: IO Bool
props = checkParallel $ Group "Gossip.Membership"
    [ ("prop_disconnected",       propDisconnected)
    , ("prop_circular_connected", propCircularConnected)
    , ("prop_connected",          propConnected)
    ]

propDisconnected :: Property
propDisconnected = property $ do
    seed <- forAll Gen.splitMixSeed
    boot <- forAll $ Gen.disconnectedContacts Gen.defaultNetworkBounds
    activeDisconnected seed boot

propCircularConnected :: Property
propCircularConnected = property $ do
    seed <- forAll Gen.splitMixSeed
    boot <- forAll $ Gen.circularContacts Gen.defaultNetworkBounds
    activeConnected seed boot

propConnected :: Property
propConnected = property $ do
    seed <- forAll Gen.splitMixSeed
    boot <- forAll $ Gen.connectedContacts Gen.defaultNetworkBounds
    activeConnected seed boot

-- | Bootstrap the protocol with the respective contacts given by 'Contacts',
-- and assert the network of active views converged to a connected state.
activeConnected :: SplitMixSeed -> Contacts -> PropertyT IO ()
activeConnected seed boot = do
    peers <- lift $ runNetwork seed boot
    annotateShow $ passiveNetwork peers
    assert $ isConnected (activeNetwork peers)

-- | Like 'propActiveConnected', but assert that the network converges to a
-- disconnected state.
--
-- This exists to suppress output which 'Test.Tasty.ExpectedFailure.expectFail'
-- would produce, and also because there's no point in letting hedgehog shrink
-- on failure.
activeDisconnected :: SplitMixSeed -> Contacts -> PropertyT IO ()
activeDisconnected seed boot = do
    peers <- lift $ runNetwork seed boot
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

runNetwork :: SplitMixSeed -> Contacts -> IO [(NodeId, Peers NodeId)]
runNetwork seed boot = do
    nodes <- Map.fromList <$> initNodes seed init'

    for_ (zip contacts (Map.elems nodes)) $ \(cs, hdl) ->
        runMembership nodes hdl $ joinAny cs

    fmap Map.toList . for nodes $ \hdl ->
        runMembership nodes hdl getPeers'
  where
    (init', contacts) = unzip boot

initNodes :: SplitMixSeed -> [NodeId] -> IO [(NodeId, Node)]
initNodes seed ns =
    for ns $ \n -> (n,) <$> new n defaultConfig (seedSMGen' seed)

runMembership :: Nodes -> Handle NodeId -> HyParView NodeId a -> IO a
runMembership nodes hdl ma = runHyParView hdl ma >>= eval
  where
    eval = \case
        ConnectionOpen to k ->
            k (Right (mkConn to)) >>= eval

        SendAdHoc rpc k -> do
            onNode (rpcRecipient rpc) $ receive rpc
            k >>= eval

        NeighborUp   _ k -> k >>= eval
        NeighborDown _ k -> k >>= eval

        Done a -> pure a

    mkConn to = Connection
        { connSend  = onNode to . receive
        , connClose = pure ()
        }

    onNode n ma' =
        for_ (Map.lookup n nodes) $ \hdl' -> runMembership nodes hdl' ma'
