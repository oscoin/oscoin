module Oscoin.Consensus.Tests (tests) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Test.Network
import           Oscoin.Consensus.Test.Network.Arbitrary
import           Oscoin.Consensus.Test.Node
import           Oscoin.Consensus.Test.View
import           Oscoin.Consensus.Simple

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Data.Set as Set

tests :: [TestTree]
tests =
    [ testGroup "With Partitions"
        [ testProperty "All nodes DON'T include all txns (simple test)" $
            expectFailure $ propNetworkNodesConverge (arbitraryNetwork @(TestNode DummyTx))
        , testProperty "All nodes DON'T include all txns (buffered test)" $
            expectFailure $ propNetworkNodesConverge (arbitraryNetwork @(BufferedTestNode DummyTx))
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge (arbitraryPartitionedNetwork @(SimpleNode DummyTx))
        ]
    , testGroup "Without Partitions"
        [ testProperty "All nodes include all txns (simple test)" $
            propNetworkNodesConverge (arbitraryHealthyNetwork @(TestNode DummyTx))
        , testProperty "All nodes include all txns (buffered test)" $
            propNetworkNodesConverge (arbitraryHealthyNetwork @(BufferedTestNode DummyTx))
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge (arbitraryHealthyNetwork @(SimpleNode DummyTx))
        ]
    ]

propNetworkNodesConverge
    :: forall a . (Show (TestNetwork a), TestableNode a)
    => Gen (TestNetwork a)
    -> Property
propNetworkNodesConverge testNetworks =
    forAllShrink testNetworks shrink $ \tn@(TestNetwork _ scheduled _ _ _) ->
        networkNonTrivial tn ==>
            let TestNetwork nodes _ _ log _ = runNetwork tn
                scheduledMsgs               = mapMaybe scheduledMessage
                                            $ Set.toList scheduled
                prettyLog                   = unlines $ " log:" : reverse ["  " ++ show l | l <- log]
                prettyStates                = unlines $ [" states:", "  " ++ show (map testablePostState nodes)]
                prettyNodes                 = unlines $ [" nodes:", "  " ++ show (length nodes)]
                prettyInfo                  = unlines $ [" info:", unlines ["  " ++ show (testableNodeAddr n) ++ ": " ++ testableShow n | n <- toList nodes]]

             in cover (not $ null $ testablePostState $ head $ toList nodes) 90 "replicated any data" $
                      counterexample (prettyLog ++ prettyNodes ++ prettyStates ++ prettyInfo)
                      (equal $ map testablePostState (toList nodes))
