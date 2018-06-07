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
            expectFailure $ propNetworkNodesConverge nodesMatch (arbitraryNetwork @(TestNode DummyTx))
        , testProperty "All nodes DON'T include all txns (buffered test)" $
            expectFailure $ propNetworkNodesConverge nodesMatch (arbitraryNetwork @(BufferedTestNode DummyTx))
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge nodePrefixesMatch (arbitraryPartitionedNetwork @(SimpleNode DummyTx))
        ]
    , testGroup "Without Partitions"
        [ testProperty "All nodes include all txns (simple test)" $
            propNetworkNodesConverge nodesMatch (arbitraryHealthyNetwork @(TestNode DummyTx))
        , testProperty "All nodes include all txns (buffered test)" $
            propNetworkNodesConverge nodesMatch (arbitraryHealthyNetwork @(BufferedTestNode DummyTx))
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge nodePrefixesMatch (arbitraryHealthyNetwork @(SimpleNode DummyTx))
        , testProperty "All nodes include all txns (nakamoto)" $
            propNetworkNodesConverge nodePrefixesMatch (arbitraryHealthyNetwork @(Nakamoto DummyTx))
        ]
    ]

propNetworkNodesConverge
    :: forall a . (Show (TestNetwork a), TestableNode a)
    => (Map (Addr a) a -> Bool)
    -> Gen (TestNetwork a)
    -> Property
propNetworkNodesConverge stateCmp testNetworks =
    forAllShrink testNetworks shrink $ \tn ->
        networkNonTrivial tn ==>
            let TestNetwork nodes _ _ log _ msgCount = runNetwork tn
                prettyLog                            = unlines $ " log:" : reverse ["  " ++ show l | l <- reverse $ sort log]
                prettyStates                         = unlines $ [" states:", "  " ++ show (map testablePostState nodes)]
                prettyNodes                          = unlines $ [" nodes:", "  " ++ show (length nodes)]
                prettyInfo                           = unlines $ [" info:", unlines ["  " ++ show (testableNodeAddr n) ++ ": " ++ testableShow n | n <- toList nodes]]
                filteredInitialMsgs                  = Set.filter isMsg (tnMsgs tn)
                msgAmp                               = msgCount `div` length filteredInitialMsgs
                prettyMsgAmp                         = unlines $ [" message amplification:" ++ show msgAmp]

             in cover (not $ null $ testablePostState $ head $ toList nodes) 90 "replicated any data" $
                      counterexample (prettyLog ++ prettyNodes ++ prettyStates ++ prettyInfo ++ prettyMsgAmp)
                                     (stateCmp nodes && msgAmp <= maximumMsgAmp)
  where
    maximumMsgAmp = 9000

nodesMatch :: TestableNode a => Map (Addr a) a -> Bool
nodesMatch nodes = equal $ map testablePostState (toList nodes)

nodePrefixesMatch :: TestableNode a => Map (Addr a) a -> Bool
nodePrefixesMatch nodes =
    let states = map testablePostState (toList nodes)
        minLen = commonPrefixLen states
        shorts = map (take minLen) states
     in equal shorts

commonPrefixLen :: Eq a => [[a]] -> Int
commonPrefixLen [] = 0
commonPrefixLen xs = length (go [] xs)
  where
    go :: Eq a => [a] -> [[a]] -> [a]
    go common ass
        | [] `elem` ass = common
        | equal heads   = go common' tails
        | otherwise     = common
      where
        common' = head heads : common
        heads   = map head ass
        tails   = map tail ass
