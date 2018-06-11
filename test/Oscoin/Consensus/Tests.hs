module Oscoin.Consensus.Tests (tests) where

import           Oscoin.Prelude

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
            expectFailure $ propNetworkNodesConverge nodesMatch anyAmp (arbitraryNetwork @(TestNode DummyTx))
        , testProperty "All nodes DON'T include all txns (buffered test)" $
            expectFailure $ propNetworkNodesConverge nodesMatch anyAmp (arbitraryNetwork @(BufferedTestNode DummyTx))
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge nodePrefixesMatch (<= 2) (arbitraryPartitionedNetwork @(SimpleNode DummyTx))
        ]
    , testGroup "Without Partitions"
        [ testProperty "All nodes include all txns (simple test)" $
            propNetworkNodesConverge nodesMatch anyAmp (arbitraryHealthyNetwork @(TestNode DummyTx))
        , testProperty "All nodes include all txns (buffered test)" $
            propNetworkNodesConverge nodesMatch anyAmp (arbitraryHealthyNetwork @(BufferedTestNode DummyTx))
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge nodePrefixesMatch (<= 2) (arbitraryHealthyNetwork @(SimpleNode DummyTx))
        , testProperty "All nodes include all txns (nakamoto)" $
            propNetworkNodesConverge nodePrefixesMatch (<= 3) (arbitraryHealthyNetwork @(Nakamoto DummyTx))
        ]
    ]
  where
    anyAmp = const True

propNetworkNodesConverge
    :: forall a . (Show (TestNetwork a), TestableNode a)
    => (TestNetwork a -> Bool)
    -> (Int -> Bool)
    -> Gen (TestNetwork a)
    -> Property
propNetworkNodesConverge stateCmp msgAmpPred testNetworks =
    forAllShrink testNetworks shrink $ \tn ->
        networkNonTrivial tn ==>
            let tn'           = runNetwork tn
                msgAmp        = msgAmplification tn' msgsSent
                initialMsgs   = Set.filter isMsg (tnMsgs tn)
                msgsSent      = tnMsgCount tn' - length initialMsgs

             in cover (not . null . testableIncludedTxs . head . toList $ tnNodes tn') 60 "replicated any data" $
                 counterexample (prettyCounterexample tn' msgAmp msgsSent)
                                (stateCmp tn' && msgAmpPred msgAmp)

msgAmplification :: TestableNode a => TestNetwork a -> Int -> Int
msgAmplification tn@TestNetwork{..} msgsSent =
    if expectedMsgs == 0 then 0 else msgsSent `div` expectedMsgs
  where
    expectedMsgs = nodes * prefix
    nodes        = length tnNodes
    prefix       = length $ commonPrefix $ nodePrefixes tn

prettyCounterexample :: TestableNode a => TestNetwork a -> Int -> Int -> String
prettyCounterexample tn@TestNetwork{..} msgAmp msgsSent =
    prettyLog ++ prettyNodes ++ prettyInfo ++ prettyMsgAmp
  where
    prettyLog    = unlines $  " log:" : reverse ["  " ++ show l | l <- reverse $ sort tnLog]
    prettyNodes  = unlines $ [" nodes:", "  " ++ show (length tnNodes)]
    prettyInfo   = unlines $ [" info:", unlines ["  " ++ show (testableNodeAddr n) ++ ": " ++ testableShow n | n <- toList tnNodes]]
    prettyMsgAmp = unlines $ [" msgs sent: "     ++ show msgsSent,
                              " common prefix: " ++ show (length $ commonPrefix $ nodePrefixes tn),
                              " amplification: " ++ show msgAmp ++ "x" ]

nodesMatch :: TestableNode a => TestNetwork a -> Bool
nodesMatch TestNetwork{..} = equal $ map testablePostState (toList tnNodes)

nodePrefixesMatch :: TestableNode a => TestNetwork a -> Bool
nodePrefixesMatch =
    not . null .commonPrefix . nodePrefixes

nodePrefixes :: TestableNode a => TestNetwork a -> [[TestableResult a]]
nodePrefixes TestNetwork{..} =
    map (reverse . testablePostState) (toList tnNodes)

commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix [] = []
commonPrefix xs = go [] xs
  where
    go :: Eq a => [a] -> [[a]] -> [a]
    go common ass
        | [] `elem` ass = common
        | equal heads   = head heads : go common tails
        | otherwise     = common
      where
        heads   = map head ass
        tails   = map tail ass
