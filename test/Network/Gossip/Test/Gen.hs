module Network.Gossip.Test.Gen
    ( NodeId
    , Contacts
    , NetworkBounds
    , SplitMixSeed
    , defaultNetworkBounds
    , connectedContacts
    , disconnectedContacts
    , circularContacts
    , splitMixSeed
    ) where

import           Prelude

import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.Class as Alga (toGraph)
import qualified Algebra.Graph.Relation.Symmetric as Sym (toRelation)
import           Control.Applicative (liftA2)
import           Data.Bifunctor (second)
import qualified Data.Graph as Graph
import           Data.List (uncons, unfoldr)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word16, Word64)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-# ANN module ("HLint: ignore Use map" :: String) #-}

type NodeId   = Word16
type Contacts = [(NodeId, [NodeId])]

data NetworkBounds = NetworkBounds
    { netMinNodes    :: Int
    , netMaxNodes    :: Int
    , netMaxContacts :: Int
    }

defaultNetworkBounds :: NetworkBounds
defaultNetworkBounds = NetworkBounds
    { netMinNodes    = 5
    , netMaxNodes    = 100
    , netMaxContacts = 5
    }

connectedContacts :: MonadGen m => NetworkBounds -> m Contacts
connectedContacts bounds = do
    nodes  <- nodeIds bounds
    splits <- Gen.list (Range.singleton (Set.size nodes))
                       (Gen.int (Range.constant 1 (netMaxContacts bounds)))
    graph  <-
        fmap Alga.overlays . traverse subgraph $
            clusters (Set.toList nodes, splits)
    pure $ Alga.adjacencyList (ensureConnected graph)
  where
    -- Split network into randomly-sized chunks.
    clusters :: ([NodeId], [Int]) -> [[NodeId]]
    clusters = unfoldr $ \case
        ([], _)    -> Nothing
        (ns, [])   -> Just (ns, mempty)
        (ns, s:ss) -> let (h, t) = splitAt s ns in Just (h, (t, ss))

    genTopo = Gen.element
        [ Alga.path
        , Alga.circuit
        , Alga.clique
        , maybe Alga.empty (uncurry Alga.star) . uncons
        ]

    subgraph [node] = pure $ Alga.vertex node
    subgraph nodes  = ($ nodes) <$> genTopo

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

disconnectedContacts :: MonadGen m => NetworkBounds -> m Contacts
disconnectedContacts bounds = map (,[]) . Set.toList <$> nodeIds bounds

circularContacts :: MonadGen m => NetworkBounds -> m Contacts
circularContacts bounds =
    toContacts . zipped . Set.toList <$> nodeIds bounds
  where
    zipped :: [NodeId] -> [(NodeId, NodeId)]
    zipped ns = zip ns $ drop 1 (cycle ns)

    toContacts :: [(NodeId, NodeId)] -> Contacts
    toContacts = map (second pure)

nodeIds :: MonadGen m => NetworkBounds -> m (Set NodeId)
nodeIds NetworkBounds{..} =
    Gen.set (Range.constantFrom netMinNodes netMinNodes netMaxContacts)
            (nodeId netMaxNodes)

nodeId :: MonadGen m => Int -> m NodeId
nodeId maxNodes = Gen.word16 (Range.constant 0 (fromIntegral $ maxNodes - 1))

type SplitMixSeed = (Word64, Word64)

splitMixSeed :: MonadGen m => m SplitMixSeed
splitMixSeed = liftA2 (,) word64 word64
  where
    word64 = Gen.prune $ Gen.word64 Range.constantBounded
