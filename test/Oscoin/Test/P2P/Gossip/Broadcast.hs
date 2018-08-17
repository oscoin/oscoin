module Oscoin.Test.P2P.Gossip.Broadcast
    ( tests

    , propAtomicBroadcast

    , genBroadcasts

    , initNodes
    , cast
    , settle

    , bootTopo
    , eagerTopo
    , lazyTopo
    , renderTopo
    ) where

import           Oscoin.Prelude

import           Oscoin.P2P.Gossip.Broadcast

import           Oscoin.Test.P2P.Gossip.Assert (allEqual)
import           Oscoin.Test.P2P.Gossip.Gen (Contacts, NodeId)
import qualified Oscoin.Test.P2P.Gossip.Gen as Gen

import           Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.Export.Dot as Alga (exportViaShow)
import           Control.Monad ((>=>))
import           Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog

type Node  = (Handle NodeId, Store)
type Nodes = Map NodeId Node
type Store = IORef (Map MessageId ByteString)

tests :: TestTree
tests = testGroup "Broadcast"
    [ testGroup "Static Network"
        [ testProperty "Atomic Broadcast" . property $ do
            boot   <- forAll $ Gen.connectedContacts Gen.defaultNetworkBounds
            bcasts <- forAll $ genBroadcasts boot
            nodes  <- lift $ initNodes boot
            propAtomicBroadcast nodes bcasts
        ]
    ]

propAtomicBroadcast
    :: Nodes
    -> [(Int, (MessageId, ByteString))]
    -> PropertyT IO ()
propAtomicBroadcast nodes bcasts = do
    stores <- lift $ do
        traverse_ (cast nodes) bcasts
        Map.toList <$> traverse (readIORef . snd) nodes
    annotateShow stores
    assert $
        let stores' = map snd stores
            bcasts' = map snd bcasts
         in allEqual stores' && head stores' == Map.fromList bcasts'

cast :: Nodes -> (Int, (MessageId, ByteString)) -> IO ()
cast nodes (root, bcast) = do
    let (hdl, store) = snd $ Map.elemAt root nodes
    void $ uncurry (storeInsert store) bcast
    out <- runPlumtree hdl $ uncurry broadcast bcast
    settle nodes out

genBroadcasts :: MonadGen m => Contacts -> m [(Int, (MessageId, ByteString))]
genBroadcasts (length -> glen) = do
    roots  <- Gen.nonEmpty (Range.linear 1 glen) (Gen.element [0..(glen - 1)])
    bcasts <-
        Gen.list (Range.singleton (length roots)) $
            liftA2 (,) (Gen.prune $ Gen.bytes (Range.singleton 8))
                       (Gen.prune $ Gen.bytes (Range.singleton 32))

    pure $ zip (toList roots) bcasts

settle :: Nodes -> [Outgoing NodeId] -> IO ()
settle nodes outs = loop $ pure outs
  where
    loop = maybe (pure ()) (foldMap dispatch >=> loop)

    dispatch (Eager to msg)   = onNode to $ receive msg
    dispatch (Lazy  to ihave) = onNode to $ receive (IHaveM ihave)
    dispatch (After 0 _ ma)   = Just <$> io ma
    dispatch (After t k ma)   = pure $ Just [After (t - 1000000) k ma]
    dispatch _                = pure mempty

    onNode n ma = for (Map.lookup n nodes) $ \(hdl,_) -> runPlumtree hdl ma

initNodes :: Contacts -> IO Nodes
initNodes net = do
    nodes <-
        for net $ \(self, peers) -> do
            store <- newIORef mempty
            hdl   <- new self (Set.fromList peers) (simpleCallbacks store)
            pure (self, (hdl, store))
    pure $ Map.fromList $ nodes

simpleCallbacks :: Store -> Callbacks
simpleCallbacks ref = Callbacks {..}
  where
    applyMessage  = storeInsert ref
    lookupMessage = storeLookup ref

storeInsert :: Store -> MessageId -> ByteString -> IO ApplyResult
storeInsert ref mid payload = atomicModifyIORef' ref $ \m ->
    case Map.insertLookupWithKey (\_ v _ -> v) mid payload m of
        (Nothing, m') -> (m', Applied)
        (Just  _, _ ) -> (m , Stale)

storeLookup :: Store -> MessageId -> IO (Maybe ByteString)
storeLookup ref mid = Map.lookup mid <$> readIORef ref

bootTopo :: Contacts -> AdjacencyMap NodeId
bootTopo = Alga.fromAdjacencyList

eagerTopo :: Nodes -> IO (AdjacencyMap NodeId)
eagerTopo nodes = do
    adj <-
        for nodes $ \(hdl,_) ->
            runPlumtree hdl $ Set.toList <$> eagerPushPeers
    pure . Alga.fromAdjacencyList . Map.toList $ adj

lazyTopo :: Nodes -> IO (AdjacencyMap NodeId)
lazyTopo nodes = do
    adj <-
        for nodes $ \(hdl,_) ->
            runPlumtree hdl $ Set.toList <$> lazyPushPeers
    pure . Alga.fromAdjacencyList . Map.toList $ adj

renderTopo :: (IsString s, Monoid s, Eq s) => AdjacencyMap NodeId -> s
renderTopo = Alga.exportViaShow
