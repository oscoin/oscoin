module Oscoin.Consensus.Tests (tests) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Test.Network
import           Oscoin.Consensus.Test.Network.Arbitrary
import           Oscoin.Consensus.Test.Node
import           Oscoin.Consensus.Test.View
import           Oscoin.Consensus.Simple

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.Maybe (catMaybes)
import qualified Data.Set as Set

tests :: [TestTree]
tests =
    [ testGroup "With Partitions"
        [ testProperty "All nodes DON'T include all txns (simple test)" $
            expectFailure $ propNetworkNodesIncludeAllTxns (arbitraryNetwork @(TestNode DummyTx))
        , testProperty "All nodes DON'T include all txns (buffered test)" $
            expectFailure $ propNetworkNodesIncludeAllTxns (arbitraryNetwork @(BufferedTestNode DummyTx))
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesIncludeAllTxns (arbitraryNetwork @(SimpleNode DummyTx))
        ]
    , testGroup "Without Partitions"
        [ testProperty "All nodes include all txns (simple test)" $
            propNetworkNodesIncludeAllTxns (arbitraryHealthyNetwork @(TestNode DummyTx))
        , testProperty "All nodes include all txns (buffered test)" $
            propNetworkNodesIncludeAllTxns (arbitraryHealthyNetwork @(BufferedTestNode DummyTx))
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesIncludeAllTxns (arbitraryHealthyNetwork @(SimpleNode DummyTx))
        ]
    ]

propNetworkNodesIncludeAllTxns
    :: (Show (TestNetwork a), Ord (TestableTx a), TestableNode a, TestableTx a ~ Msg a)
    => Gen (TestNetwork a)
    -> Property
propNetworkNodesIncludeAllTxns testNetworks =
    forAllShrink testNetworks shrink $ \tn@(TestNetwork _ scheduled _) ->
        networkNonTrivial tn ==>
            let TestNetwork nodes _ _ = runNetwork tn
                scheduledMsgs         = catMaybes . Set.toList . Set.map scheduledMessage
                                      $ scheduled
             in all (\ns -> testableNodeState ns == scheduledMsgs) (toList nodes)
