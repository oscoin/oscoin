{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Oscoin.Test.P2P.Gossip.Membership.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.P2P.Gossip.Membership

import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.Class as Alga (toGraph)
import qualified Algebra.Graph.Export.Dot as Alga (exportViaShow)
import qualified Algebra.Graph.Relation.Symmetric as Sym (toRelation)
import           Data.Bifunctor (second)
import           Data.Graph (Forest, graphFromEdges')
import qualified Data.Graph as Graph
import           Data.List (unfoldr)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified System.Random.MWC as MWC

import           Test.QuickCheck

newtype Seed = Seed { fromSeed :: MWC.Seed }
    deriving Show

instance Arbitrary Seed where
    arbitrary = Seed . MWC.toSeed . Vector.fromList <$> vectorOf 258 arbitrary
    shrink    = shrinkNothing

newtype NonEmptySet a = NonEmptySet { getNonEmptySet :: Set a }
    deriving Generic

instance (Arbitrary a, Ord a) => Arbitrary (NonEmptySet a) where
    arbitrary =
        (NonEmptySet . Set.fromList <$> listOf1 arbitrary)
            `suchThat` (not . Set.null . getNonEmptySet)
    shrink = genericShrink

type NodeId = Word16

newtype Network = Network { fromNetwork :: [(NodeId, [NodeId])] }
    deriving (Eq, Show, Read, Generic)

instance Semigroup Network where
    a <> b = Network $ fromNetwork a <> fromNetwork b

instance Monoid Network where
    mempty  = Network mempty
    mappend = (<>)

-- | The 'Network' is connected if its graph has exactly one component.
isConnected :: Network -> Bool
isConnected = (== 1) . length . components

components :: Network -> Forest NodeId
components (Network adj) = map (fstOf3 . l) <$> Graph.components gv
  where
    (gv, l) = graphFromEdges' $ map dupFst adj

    dupFst (a, b)    = (a, a, b)
    fstOf3 (a, _, _) = a

renderNetworkDot :: Network -> String
renderNetworkDot = Alga.exportViaShow . Alga.fromAdjacencyList . fromNetwork

activeNetwork :: [(NodeId, Peers NodeId a)] -> Network
activeNetwork = Network . map (second (Map.keys . active))

passiveNetwork :: [(NodeId, Peers NodeId a)] -> Network
passiveNetwork = Network . map (second (Set.toList . passive))

-- | Non-empty 'Network' in which no node is connected.
arbitraryDisconnectedNetwork :: Gen Network
arbitraryDisconnectedNetwork =
    Network . map (,[]) . Set.toList . getNonEmptySet <$> arbitrary

-- | Non-empty 'Network' in which each node knows its right neighbour, forming a
-- cycle.
arbitraryCircularNetwork :: Gen Network
arbitraryCircularNetwork =
    toNetwork . zipped . map fst . fromNetwork <$> arbitraryDisconnectedNetwork
  where
    toNetwork :: [(NodeId, NodeId)] -> Network
    toNetwork = Network . map (second (:[]))

    zipped :: [NodeId] -> [(NodeId, NodeId)]
    zipped ns = zip ns $ drop 1 (cycle ns)

-- | Non-empty, connected 'Network'.
--
-- This attempts to create somewhat more interesting topologies than
-- 'arbitraryCircularNetwork'.
arbitraryConnectedNetwork :: Gen Network
arbitraryConnectedNetwork = do
    nodes  <- Set.toList . getNonEmptySet <$> arbitrary
    splits <- infiniteListOf (choose (1, maxContacts))
    graph  <- map Alga.overlays . traverse subgraph $ clusters (nodes, splits)

    pure . Network . Alga.adjacencyList $ ensureConnected graph
  where
    -- Note: technically, this could be max active + max passive, but that tends
    -- to create "disconnect storms", which aren't very interesting for our
    -- purposes.
    maxContacts = fromIntegral $ cfgMaxActive defaultConfig

    -- Split network into randomly-sized chunks.
    clusters :: ([NodeId], [Int]) -> [[NodeId]]
    clusters = unfoldr $ \case
        ([], _)  -> Nothing
        (ns, ss) -> let (h, t) = splitAt (head ss) ns
                     in Just (h, (t, tail ss))

    genTopo = elements
        [ Alga.path
        , Alga.circuit
        , Alga.clique
        , uncurry Alga.star . uncons'
        ]

    subgraph nodes = do
        topo <- genTopo
        pure $ topo nodes

    uncons' xs = (head xs, tail xs)

    -- Ensure the graph is connected: if it has only one component, it is
    -- already connected, otherwise, connect the roots of the forest as a
    -- (undirected) circuit and overlay the result onto the graph.
    ensureConnected g =
        let gu = undirected g
         in case Alga.dfsForest gu of
                cs@(_:_:_) -> Alga.overlay gu . undirected . Alga.circuit
                            $ map tip cs
                _          -> gu

    undirected = Alga.toGraph . Sym.toRelation . Alga.toGraph

    tip :: Graph.Tree a -> a
    tip (Graph.Node a _) = a
