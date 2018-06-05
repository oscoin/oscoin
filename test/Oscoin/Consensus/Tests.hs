module Oscoin.Consensus.Tests (tests) where

import           Oscoin.Prelude hiding (log)

import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Test.Network
import           Oscoin.Consensus.Test.Network.Arbitrary
import           Oscoin.Consensus.Test.Node
import           Oscoin.Consensus.Test.View
import           Oscoin.Consensus.Simple
import           Oscoin.Consensus.Nakamoto (Nakamoto)

import           Data.List (sort)
import qualified Data.Set as Set

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

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
        , testProperty "All nodes include all txns (nakamoto)" $
            propNetworkNodesConverge (arbitraryHealthyNetwork @(Nakamoto DummyTx))
        ]
    ]

propNetworkNodesConverge
    :: forall a . (Show (TestNetwork a), TestableNode a)
    => Gen (TestNetwork a)
    -> Property
propNetworkNodesConverge testNetworks =
    forAllShrink testNetworks shrink $ \tn ->
        networkNonTrivial tn ==>
            let TestNetwork nodes _ _ log _ msgCount = runNetwork tn
                prettyLog                            = unlines $ " log:" : reverse ["  " ++ show l | l <- reverse $ sort log]
                prettyStates                         = unlines $ [" states:", "  " ++ show (map testablePostState nodes)]
                prettyNodes                          = unlines $ [" nodes:", "  " ++ show (length nodes)]
                prettyInfo                           = unlines $ [" info:", unlines ["  " ++ show (testableNodeAddr n) ++ ": " ++ testableShow n | n <- toList nodes]]
                filteredInitialMsgs                  = Set.filter (not . isTick) (tnMsgs tn)
                msgAmp                               = msgCount `div` (1 + length filteredInitialMsgs)
                prettyMsgAmp                         = unlines $ [" message amplification:" ++ show msgAmp]

             in cover (not $ null $ testablePostState $ head $ toList nodes) 90 "replicated any data" $
                      counterexample (prettyLog ++ prettyNodes ++ prettyStates ++ prettyInfo ++ prettyMsgAmp)
                                     (nodesMatch nodes && msgAmp <= 9000)

nodesMatch :: TestableNode a => Map (Addr a) a -> Bool
nodesMatch nodes =
    let states = map testablePostState (toList nodes)
        minLen = length $ minimumBy (\a b -> compare (length a) (length b)) states
        shorts = map (\s -> take minLen s) states
     in equal shorts
