module Oscoin.Consensus.Tests where

import           Oscoin.Prelude

import           Oscoin.Consensus.Test.Network
import           Oscoin.Consensus.Test.Network.Arbitrary

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import qualified Oscoin.Consensus.Simple as Simple

import           Data.List (sort, isPrefixOf)

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testGroup "With Partitions"
        [ testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge @SimpleNodeState
                                     testableInit
                                     constant
                                     (arbitraryPartitionedNetwork Simple.epochLength)
        ]
    , testGroup "Without Partitions"
        [ testProperty "All nodes include all txns (simple fault tolerant)" $
            propNetworkNodesConverge @SimpleNodeState
                                     testableInit
                                     constant
                                     (arbitraryHealthyNetwork Simple.epochLength)
        , testProperty "All nodes include all txns (nakamoto)" $
            propNetworkNodesConverge @NakamotoNodeState
                                     testableInit
                                     constant
                                     (arbitraryHealthyNetwork Nakamoto.epochLength)
        ]
    ]
  where
    constant = identity

-- 1. Message amplification: we would like to measure the communication complexity
-- of the consensus protocol. For this, we count the number of blocks agreed on
-- (shared prefix) and the number of blocks sent between nodes.
propNetworkNodesConverge
    :: TestableNode a
    => (TestNetwork () -> TestNetwork a)
    -> (Int -> Int)            -- ^ Predicate on message amplification
    -> Gen (TestNetwork ())    -- ^ Network generator
    -> Property
propNetworkNodesConverge tnInit msgComplexity genNetworks =
    forAllShrink genNetworks shrink $ \tn ->
        networkNonTrivial tn ==>
            let tn'             = runNetwork (tnInit tn)
                msgsSent        = tnMsgCount tn'

                -- Nb.: All nodes have to know all txs, thus the coverage
                -- condition only needs to check one node.
                replicatedTxs   = testableIncludedTxs . head . toList $ tnNodes tn'
                nodeCount       = length $ tnNodes tn
                chainLength     = length (commonPrefix (nodePrefixes tn'))
                minMsgs         = (chainLength - 1) * msgComplexity nodeCount             - (chainLength - 1)
                maxMsgs         = (chainLength - 1) * msgComplexity nodeCount * nodeCount - (chainLength - 1)

                -- Predicates.
                chainGrowthP    = chainLength > 1
                msgComplexityP  = msgsSent >= minMsgs && msgsSent <= maxMsgs
                commonPrefixP   = if nodeCount > 1 then nodePrefixesMatch tn' else True
                replicationP    = not . null $ replicatedTxs
                majorityPrefixP = majorityNodePrefixesMatch tn'

                -- TODO(cloudhead): Document.
             in cover replicationP   70 "replicated any data"
                -- TODO(cloudhead): Document.
              . cover chainGrowthP   75 "have more than one block"
                -- The expected minimum amount of messages sent is the length of the shared
                -- prefix times the number of nodes. Anything above that is considered
                -- 'amplification'.
              . cover msgComplexityP 75 "are within the communication complexity bounds"
                -- TODO(cloudhead): Document.
              . cover commonPrefixP  95 "have a common prefix"
              . counterexample (prettyCounterexample tn' msgsSent (length replicatedTxs))
                -- TODO(cloudhead): Document.
              $ majorityPrefixP

prettyCounterexample :: TestableNode a => TestNetwork a -> Int -> Int -> String
prettyCounterexample tn@TestNetwork{..} msgsSent txsReplicated =
    prettyLog ++ prettyInfo ++ prettyNodes ++ prettyStats
  where
    prettyLog    = unlines $  " log:" : reverse ["  " ++ show l | l <- reverse $ sort tnLog]
    prettyNodes  = unlines $ [" nodes:", "  " ++ show (length tnNodes)]
    prettyInfo   = unlines $ [" info:", unlines ["  " ++ show (testableNodeAddr n) ++ ": " ++ testableShow n | n <- toList tnNodes]]
    prettyStats  = unlines $ [" msgs sent: "      ++ show msgsSent,
                              " txs replicated: " ++ show txsReplicated,
                              " common prefix: "  ++ show (length $ commonPrefix $ nodePrefixes tn) ]

-- TODO(cloudhead): Document.
nodePrefixesMatch :: TestableNode a => TestNetwork a -> Bool
nodePrefixesMatch =
    (> 1) . length . commonPrefix . nodePrefixes

-- TODO(cloudhead): Document.
majorityNodePrefixesMatch :: TestableNode a => TestNetwork a -> Bool
majorityNodePrefixesMatch tn@TestNetwork{..} =
    length ns > length tnNodes - length ns
  where
    pre = commonPrefix $ nodePrefixes tn
    ns  = filter (nodeHasPrefix pre) (toList tnNodes)

-- TODO(cloudhead): Document.
nodeHasPrefix :: TestableNode a => [TestableResult a] -> a -> Bool
nodeHasPrefix p node =
    p `isPrefixOf` reverse (testablePostState node)

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
