module Oscoin.Test.Consensus where

import           Oscoin.Prelude

import           Oscoin.Time
import           Oscoin.Test.Consensus.Nakamoto
import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Network.Arbitrary
import           Oscoin.Test.Consensus.Node (DummyTx)
import           Oscoin.Test.Consensus.Simple

import           Oscoin.Consensus.BlockStore
                 (genesisBlockStore, insert, maximumChainBy, orphans)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import qualified Oscoin.Consensus.Simple as Simple
import           Oscoin.Crypto.Blockchain (blockHash, tip)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block
                 , BlockHash
                 , blockPrevHash
                 , blockTimestamp
                 , emptyGenesisBlock
                 , emptyHeader
                 , mkBlock
                 )

import           Data.Function (on)
import           Data.List (foldr1, isPrefixOf, sort, unlines)
import qualified Data.Text as T

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testGroup "With Partitions"
        [ testProperty "Nodes converge (simple)" $
            propNetworkNodesConverge @SimpleNodeState
                                     testableInit
                                     (arbitraryPartitionedNetwork Simple.blockTime)
        ]
    , testGroup "Without Partitions"
        [ testProperty "Nodes converge (simple)" $
            propNetworkNodesConverge @SimpleNodeState
                                     testableInit
                                     (arbitraryHealthyNetwork Simple.blockTime)
        , testProperty "Nodes converge (nakamoto)" $
            propNetworkNodesConverge @NakamotoNodeState
                                     testableInit
                                     (arbitraryHealthyNetwork Nakamoto.blockTime)
        ]
    , testGroup "BlockStore"
        [ testCase "'insert' puts blocks with parents on a chain" $ do
            let genBlk = emptyGenesisBlock epoch () :: Block () ()
                nextBlk = mkBlock emptyHeader
                    { blockTimestamp = fromEpoch (1 * seconds)
                    , blockPrevHash = blockHash genBlk
                    } []
                blkStore = insert (const . Just <$> nextBlk) $ genesisBlockStore genBlk
                chain = maximumChainBy (compare `on` length) blkStore
            tip chain @?= nextBlk
            orphans blkStore @?= mempty

        ]
    ]

propNetworkNodesConverge
    :: forall a m. TestableNode m a
    => (TestNetwork () -> TestNetwork a) -- ^ Network initialization function
    -> Gen (TestNetwork ())              -- ^ TestNetwork generator
    -> Property
propNetworkNodesConverge tnInit genNetworks =
    forAllShrink genNetworks shrink $ \tn ->
        networkNonTrivial tn ==>
            let tn'             = runNetwork (tnInit tn)

                -- Nb.: All nodes have to know all txs, thus the coverage
                -- condition only needs to check one node.
                replicatedTxs   = fromMaybe mempty . map testableIncludedTxs . head . toList $ tnNodes tn'
                nodeCount       = length (tnNodes tn')
                -- Nb.: The right metric for chain length here is not obvious, but
                -- it's unlikely that longest/shortest chain works, and also unlikely
                -- that commonPrefix works. Perhaps this is a case for using majority prefix,
                -- although this can also be solved properly by waiting for networks to
                -- converge.
                chainLength     = length (longestChain tn')
                -- We therefore expect the messaging complexity to be between O(n) and O(n^2).

                --
                -- Properties to be tested
                --
                -- Ensure that we have a minimum of chain growth. This means nodes need
                -- to have agreed on blocks beyond the genesis, which all nodes start
                -- with.
                propChainGrowth    = chainLength > 1
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
              . cover propUnanimtyPrefix  75 "have a common prefix"
              . counterexample (prettyCounterexample tn' replicatedTxs)
              $ propMajorityPrefix

-- | Return a pretty-printed TestNetwork for counter-examples.
prettyCounterexample :: TestableNode m a => TestNetwork a -> [DummyTx] -> String
prettyCounterexample tn@TestNetwork{..} txsReplicated =
    prettyLog ++ prettyInfo ++ prettyNodes ++ prettyStats
  where
    prettyLog    = unlines $  " log:" : reverse ["  " ++ show l | l <- reverse $ sort tnLog]
    prettyNodes  = unlines $ [" nodes:", "  " ++ show (length tnNodes)]
    prettyInfo   = unlines $ [" info:", unlines ["  " ++ show (testableNodeAddr n) ++ ": " ++ T.unpack (testableShow n) | n <- toList tnNodes]]
    prettyStats  = unlines $ [" msgs sent: "      ++ show tnMsgCount,
                              " txs replicated: " ++ show (length txsReplicated),
                              " common prefix: "  ++ show (length $ longestCommonPrefix $ nodePrefixes tn) ]

nodePrefixesMatch :: TestableNode m a => TestNetwork a -> Bool
nodePrefixesMatch tn =
    (>= length (shortestChain tn) - 3) . length . longestCommonPrefix $ nodePrefixes tn

shortestChain :: TestableNode m a => TestNetwork a -> [BlockHash]
shortestChain tn = minimumBy (comparing length) (nodePrefixes tn)

longestChain :: TestableNode m a => TestNetwork a -> [BlockHash]
longestChain tn = maximumBy (comparing length) (nodePrefixes tn)

-- | A 50%+ majority of nodes in the network have a common prefix.
majorityNodePrefixesMatch :: TestableNode m a => TestNetwork a -> Bool
majorityNodePrefixesMatch tn@TestNetwork{..} =
    length ns > length tnNodes - length ns
  where
    pre = longestCommonPrefix $ nodePrefixes tn
    ns  = filter (nodeHasPrefix pre) (toList tnNodes)

-- | A node has the given prefix in its longest chain.
nodeHasPrefix :: TestableNode m a => [BlockHash] -> a -> Bool
nodeHasPrefix p node =
    p `isPrefixOf` reverse (testableLongestChain node)

-- | The longest chain prefixes of all nodes in the network.
nodePrefixes :: TestableNode m a => TestNetwork a -> [[BlockHash]]
nodePrefixes TestNetwork{..} =
    -- Nb. We reverse the list to check the prefix, since the head
    -- of the list is the tip of the chain, not the genesis.
    map (reverse . testableLongestChain) (toList tnNodes)

-- | The longest common prefix of a list of lists.
longestCommonPrefix :: Eq a => [[a]] -> [a]
longestCommonPrefix [] = []
longestCommonPrefix xs = foldr1 commonPrefix xs

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix (x:xs) (y:ys)
    | x == y = x : commonPrefix xs ys
commonPrefix _ _ = []
