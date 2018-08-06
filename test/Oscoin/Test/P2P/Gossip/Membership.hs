{-# LANGUAGE TupleSections #-}

module Oscoin.Test.P2P.Gossip.Membership (tests) where

import           Oscoin.Prelude

import           Oscoin.P2P.Gossip.Membership

import           Oscoin.Test.P2P.Gossip.Membership.Arbitrary

import           Control.Monad (unless)
import           Data.Bifunctor
import qualified System.Random.MWC as MWC

import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Membership"
    [ testGroup "Overlay convergence with healthy underlay"
        [ testProperty "Disconnected network"
            . forAll (seedAnd arbitraryDisconnectedNetwork) $ \(s,n) ->
                length (fromNetwork n) > 1 ==>
                    expectFailure $ propActiveConnected s n

        , testProperty "Circular network"
            . forAll (seedAnd arbitraryCircularNetwork)
            $ uncurry propActiveConnected

        , testProperty "Connected network"
            . forAll (seedAnd arbitraryConnectedNetwork)
            $ uncurry propActiveConnected
        ]
    ]
  where
    seedAnd = liftA2 (,) arbitrary

-- | Bootstrap the protocol with the respective contacts given by 'Network', and
-- assert the network of active views converged to a connected state.
propActiveConnected :: MWC.Seed -> Network -> Property
propActiveConnected seed net = monadicIO $ do
    nodes <- run $ runNetwork seed noopCallbacks net
    let peers = map (first hSelf) nodes
        actv  = activeNetwork peers
        pasv  = passiveNetwork peers
     in unless (isConnected actv) . fail . unlines $
            [ "Active:",  show actv, ""
            , "Passive:", show pasv, ""
            ]

--------------------------------------------------------------------------------

runNetwork :: MWC.Seed -> Callbacks NodeId c -> Network -> IO [Node NodeId c]
runNetwork seed cbs (Network net) = do
    nodes <- flip zip contacts <$> initNodes seed cbs boot
    (rpcs, nodes') <- sequenceA <$> traverse (uncurry joinNetwork) nodes
    settle nodes' rpcs
  where
    (boot, contacts) = unzip net

    joinNetwork n = runNode n . joinAny

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
