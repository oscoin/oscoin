{-# LANGUAGE TupleSections #-}

module Oscoin.Test.P2P.Gossip.Membership (tests) where

import           Oscoin.Prelude

import           Oscoin.P2P.Gossip.Membership

import           Oscoin.Test.P2P.Gossip.Gen (Contacts, NodeId)
import qualified Oscoin.Test.P2P.Gossip.Gen as Gen

import qualified Algebra.Graph.AdjacencyMap as Alga
import           Data.Bifunctor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Random.MWC as MWC

import           Hedgehog
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Membership"
    [ testGroup "Overlay convergence with healthy underlay"
        [ testProperty "Disconnected network" . property $ do
            seed <- forAll Gen.mwcSeed
            boot <- forAll $ Gen.disconnectedContacts Gen.defaultNetworkBounds
            propActiveDisconnected seed boot

        , testProperty "Circular network" . property $ do
            seed <- forAll Gen.mwcSeed
            boot <- forAll $ Gen.circularContacts Gen.defaultNetworkBounds
            propActiveConnected seed boot

        , testProperty "Connected network" . property $ do
            seed <- forAll Gen.mwcSeed
            boot <- forAll $ Gen.connectedContacts Gen.defaultNetworkBounds
            propActiveConnected seed boot
        ]
    ]

-- | Bootstrap the protocol with the respective contacts given by 'Contacts',
-- and assert the network of active views converged to a connected state.
propActiveConnected :: MWC.Seed -> Contacts -> PropertyT IO ()
propActiveConnected seed boot = do
    peers <- lift $ runNetwork' seed noopCallbacks boot
    annotateShow $ passiveNetwork peers
    assert $ isConnected (activeNetwork peers)

-- | Like 'propActiveConnected', but assert that the network converges to a
-- disconnected state.
--
-- This exists to suppress output which 'Test.Tasty.ExpectedFailure.expectFail'
-- would produce, and also because there's no point in letting hedgehog shrink
-- on failure.
propActiveDisconnected :: MWC.Seed -> Contacts -> PropertyT IO ()
propActiveDisconnected seed boot = do
    peers <- lift $ runNetwork' seed noopCallbacks boot
    annotateShow $ passiveNetwork peers
    assert $ not $ isConnected (activeNetwork peers)

--------------------------------------------------------------------------------

activeNetwork :: [(NodeId, Peers NodeId a)] -> [(NodeId, [NodeId])]
activeNetwork = map (second (Map.keys . active))

passiveNetwork :: [(NodeId, Peers NodeId a)] -> [(NodeId, [NodeId])]
passiveNetwork = map (second (Set.toList . passive))

isConnected :: [(NodeId, [NodeId])] -> Bool
isConnected adj =
    case Alga.dfsForest (Alga.fromAdjacencyList adj) of
        [_] -> True
        _   -> False

--------------------------------------------------------------------------------

runNetwork :: MWC.Seed -> Callbacks NodeId c -> Contacts -> IO [Node NodeId c]
runNetwork seed cbs net = do
    nodes <- flip zip contacts <$> initNodes seed cbs boot
    (rpcs, nodes') <- sequenceA <$> traverse (uncurry joinNetwork) nodes
    settle nodes' rpcs
  where
    (boot, contacts) = unzip net

    joinNetwork n = runNode n . joinAny

runNetwork' :: MWC.Seed -> Callbacks NodeId c -> Contacts -> IO [(NodeId, Peers NodeId c)]
runNetwork' seed cbs net = map (first hSelf) <$> runNetwork seed cbs net

settle :: (Ord n, Show n) => [Node n c] -> [RPC n] -> IO [Node n c]
settle []    _    = pure []
settle nodes []   = pure nodes
settle nodes rpcs = do
    (out, nodes') <- map sequenceA $ traverse (deliver rpcs) nodes
    case out of
        [] -> pure nodes'
        xs -> settle nodes' xs

deliver :: Ord n => [RPC n] -> Node n c -> IO ([RPC n], Node n c)
deliver rpcs n =
    runNode n . map concat
              . traverse receive
              . filter ((== hSelf (fst n)) . rpcRecipient)
              $ rpcs

initNodes :: MWC.Seed -> Callbacks NodeId c -> [NodeId] -> IO [Node NodeId c]
initNodes seed cbs ns = do
    prng <- MWC.initialize (MWC.fromSeed seed)
    pure $ map (\n -> (new n defaultConfig prng cbs, mempty)) ns

type Node n c = (Handle n c, Peers n c)

runNode :: Node n c -> HyParView n c a -> IO (a, Node n c)
runNode (hdl, peers) ma = second (hdl,) <$> runHyParView hdl peers ma

noopCallbacks :: Callbacks n ()
noopCallbacks = Callbacks
    { neighborUp   = const $ pure ()
    , neighborDown = const $ pure ()
    , connOpen     = const $ pure ()
    , connClose    = const $ pure ()
    }
