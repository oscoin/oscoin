module Oscoin.Consensus.Tests where

import           Oscoin.Prelude

import           Oscoin.Consensus.Test.Network
import           Oscoin.Consensus.Test.Network.Arbitrary

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import qualified Oscoin.Consensus.Simple as Simple

import           Data.List (sort)
import qualified Data.Set as Set

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testGroup "With Partitions"
        [ testProperty "All nodes DON'T include all txns (no consensus)" $
            expectFailure $
                propNetworkNodesConverge @NoConsensusState
                                         nodesMatch
                                         anyAmp
                                         (arbitraryPartitionedNetwork 1)
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge @SimpleNodeState
                                     nodePrefixesMatch
                                     (<= 20)
                                     (arbitraryPartitionedNetwork Simple.epochLength)
        ]
    , testGroup "Without Partitions"
        [ testProperty "All nodes include all txns (no consensus)" $
            propNetworkNodesConverge @NoConsensusState
                                     nodesMatch
                                     anyAmp
                                     (arbitraryHealthyNetwork 1)
        , testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge @SimpleNodeState
                                     nodePrefixesMatch
                                     (<= 20)
                                     (arbitraryHealthyNetwork Simple.epochLength)
        , testProperty "All nodes include all txns (nakamoto)" $
            propNetworkNodesConverge @NakamotoNodeState
                                     nodePrefixesMatch
                                     (<= 20)
                                     (arbitraryHealthyNetwork Nakamoto.epochLength)
        ]
    ]
  where
    anyAmp = const True

propNetworkNodesConverge
    :: TestableNode a
    => (TestNetwork a -> Bool) -- ^ Predicate to match on the final state
    -> (Int -> Bool)           -- ^ Predicate on message amplification
    -> Gen (TestNetwork ())    -- ^ Network generator
    -> Property
propNetworkNodesConverge stateCmp msgAmpPred genNetworks =
    forAllShrink genNetworks shrink $ \tn ->
        networkNonTrivial tn ==>
            let tn'           = runNetwork (testableInit tn)
                msgAmp        = msgAmplification tn' msgsSent
                initialMsgs   = Set.filter isMsg (tnMsgs tn)
                msgsSent      = tnMsgCount tn' - length initialMsgs
                -- Nb.: All nodes have to know all txs, thus the coverage
                -- condition only needs to check one node.
                replicatedTxs = testableIncludedTxs . head . toList $ tnNodes tn'
                chainGrowth   = length (commonPrefix (nodePrefixes tn')) > 1

             in cover (not . null $ replicatedTxs) 60 "replicated any data" $
                 cover chainGrowth 60 "have more than one block" $
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
    not . null . commonPrefix . nodePrefixes

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
