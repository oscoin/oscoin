module Oscoin.Consensus.Tests where

import           Oscoin.Prelude

import           Oscoin.Consensus.Test.Network
import           Oscoin.Consensus.Test.Network.Arbitrary
import           Oscoin.Consensus.Test.Node (DummyTx)

import           Oscoin.Consensus.Evaluator (acceptAnythingEval, applyValidExprs, rejectEverythingEval)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import qualified Oscoin.Consensus.Simple as Simple
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Crypto.Blockchain.Block (BlockHeader)

import           Data.List (sort, isPrefixOf)

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testGroup "With Partitions"
        [ testProperty "Nodes converge (simple)" $
            propNetworkNodesConverge @SimpleNodeState
                                     testableInit
                                     constant
                                     (arbitraryPartitionedNetwork Simple.epochLength)
        ]
    , testGroup "Without Partitions"
        [ testProperty "Nodes converge (simple)" $
            propNetworkNodesConverge @SimpleNodeState
                                     testableInit
                                     constant
                                     (arbitraryHealthyNetwork Simple.epochLength)
        , testProperty "Nodes converge (nakamoto)" $
            propNetworkNodesConverge @NakamotoNodeState
                                     testableInit
                                     constant
                                     (arbitraryHealthyNetwork Nakamoto.epochLength)
        ]
    , testGroup "Evaluator"
        [ testProperty "applyValidExprs does not reject valid expressions" $ \(xs :: [Int]) ->
            let (res, _) = applyValidExprs xs () acceptAnythingEval
             in if | any isLeft res ->
                       counterexample ("Expected no Lefts, got: " ++ show res) False
                   | length (rights res) /= length xs ->
                       counterexample ("Expected " ++ show (length xs) ++ " Rights, got: " ++ show res) False
                   | otherwise ->
                       let expected = [Right (x, ()) | x <- xs]
                           info = "Expected: " <> show expected <> "\nGot: " <> show res
                        in counterexample info $ res == expected

        , testProperty "applyValidExprs does not accept invalid expressions" $ \(xs :: [Int]) ->
            let (res, _) = applyValidExprs xs () rejectEverythingEval
             in if | any isRight res -> counterexample ("Expected only Lefts, got: " ++ show (map (map fst) res)) False
                   | otherwise -> property $ length res == length xs
        ]
    ]
  where
    constant = identity

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
                nodeCount       = length (tnNodes tn')
                chainLength     = length (commonPrefix (nodePrefixes tn'))
                -- The expected minimum amount of messages sent is the length of the shared
                -- prefix times the number of nodes. Anything above that is considered
                -- 'amplification'. The `- 1` is to not consider the genesis block as part
                -- of the calculation, and the reason we remove `chainLength - 1` messages
                -- at the end is to take into account that for each replicated block,
                -- one node is the block proposer and thus doesn't need to receive that block.
                minMsgs         = (chainLength - 1) * msgComplexity nodeCount
                                - (chainLength - 1)
                -- The expected maximum amount of message sent is essentially the minimum *squared*.
                maxMsgs         = (chainLength - 1) * msgComplexity nodeCount * nodeCount
                                - (chainLength - 1)
                -- We therefore expect the messaging complexity to be between O(n) and O(n^2).

                --
                -- Properties to be tested
                --
                -- Ensure that we have a minimum of chain growth. This means nodes need
                -- to have agreed on blocks beyond the genesis, which all nodes start
                -- with.
                propChainGrowth    = chainLength > 1
                -- Ensure that nodes are within the communication complexity bounds.
                -- This checks that nodes are not sending too many messages to reach
                -- consensus. In Nakamoto consensus, message complexity at the network
                -- level should be O(n), where `n` is the number of nodes.
                propMsgComplexity  = msgsSent >= minMsgs && msgsSent <= maxMsgs
                -- Ensure that most runs end with *all* nodes sharing a common prefix.
                -- The reason we can't have the stronger guarantee is that with
                -- network delay, it's possible that a test run involving network partitions
                -- is unable to converge in time.
                propUnanimtyPrefix = nodeCount == 1 || nodePrefixesMatch tn'
                -- Ensure that nodes are replicating client transactions in their blocks.
                -- It's not enough to check that blocks are replicated, since empty
                -- blocks are considered valid.
                propReplication    = not . null $ replicatedTxs
                -- Ensure that a >50% majority of nodes share a common prefix.
                propMajorityPrefix = majorityNodePrefixesMatch tn'

             in cover propReplication     70 "replicated any data"
              . cover propChainGrowth     75 "have more than one block"
              . cover propMsgComplexity   75 "are within the communication complexity bounds"
              . cover propUnanimtyPrefix  95 "have a common prefix"
              . counterexample (prettyCounterexample tn' replicatedTxs)
              $ propMajorityPrefix

-- | Return a pretty-printed TestNetwork for counter-examples.
prettyCounterexample :: TestableNode a => TestNetwork a -> [DummyTx] -> String
prettyCounterexample tn@TestNetwork{..} txsReplicated =
    prettyLog ++ prettyInfo ++ prettyNodes ++ prettyStats
  where
    prettyLog    = unlines $  " log:" : reverse ["  " ++ show l | l <- reverse $ sort tnLog]
    prettyNodes  = unlines $ [" nodes:", "  " ++ show (length tnNodes)]
    prettyInfo   = unlines $ [" info:", unlines ["  " ++ show (testableNodeAddr n) ++ ": " ++ testableShow n | n <- toList tnNodes]]
    prettyStats  = unlines $ [" msgs sent: "      ++ show tnMsgCount,
                              " txs replicated: " ++ show (length txsReplicated),
                              " common prefix: "  ++ show (length $ commonPrefix $ nodePrefixes tn) ]

-- | All nodes in the network have more than one block in common on their
-- longest chain.
nodePrefixesMatch :: TestableNode a => TestNetwork a -> Bool
nodePrefixesMatch =
    (> 1) . length . commonPrefix . nodePrefixes

-- | A 50%+ majority of nodes in the network have a common prefix.
majorityNodePrefixesMatch :: TestableNode a => TestNetwork a -> Bool
majorityNodePrefixesMatch tn@TestNetwork{..} =
    length ns > length tnNodes - length ns
  where
    pre = commonPrefix $ nodePrefixes tn
    ns  = filter (nodeHasPrefix pre) (toList tnNodes)

-- | A node has the given prefix in its longest chain.
nodeHasPrefix :: TestableNode a => [Hashed (BlockHeader ())] -> a -> Bool
nodeHasPrefix p node =
    p `isPrefixOf` reverse (testableLongestChain node)

-- | The longest chain prefixes of all nodes in the network.
nodePrefixes :: TestableNode a => TestNetwork a -> [[Hashed (BlockHeader ())]]
nodePrefixes TestNetwork{..} =
    -- Nb. We reverse the list to check the prefix, since the head
    -- of the list is the tip of the chain, not the genesis.
    map (reverse . testableLongestChain) (toList tnNodes)

-- | The longest common prefix of a list of lists.
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
