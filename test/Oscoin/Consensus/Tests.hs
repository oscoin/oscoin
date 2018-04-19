module Oscoin.Consensus.Tests (tests) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Test.Network
import           Oscoin.Consensus.Test.Network.Arbitrary
import           Oscoin.Consensus.Test.Node
import           Oscoin.Consensus.Test.View

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.Maybe (catMaybes)
import qualified Data.Set as Set

tests :: [TestTree]
tests =
    [ testGroup "With Partitions"
        [ testProperty "All nodes include all txns" $
            expectFailure $ propNetworkNodesIncludeAllTxns (arbitraryNetwork @(TestNode DummyTx))
        , testProperty "All nodes include all txns (buffered)" $
            expectFailure $ propNetworkNodesIncludeAllTxns (arbitraryNetwork @(BufferedTestNode DummyTx))
        ]
    , testGroup "Without Partitions"
        [ testProperty "All nodes include all txns" $
            propNetworkNodesIncludeAllTxns (arbitraryHealthyNetwork @(TestNode DummyTx))
        , testProperty "All nodes include all txns (buffered)" $
            propNetworkNodesIncludeAllTxns (arbitraryHealthyNetwork @(BufferedTestNode DummyTx))
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
