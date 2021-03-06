module Oscoin.Test.Consensus
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Test.Consensus.Nakamoto
import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Network.Arbitrary
import           Oscoin.Time

import qualified Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain.Block (BlockHash)

import           Codec.Serialise (Serialise)
import           Data.List (foldr1, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import           Oscoin.Test.Crypto
import           Test.Oscoin.DummyLedger
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> Consensus.Config -> TestTree
tests Dict config = testGroup "Oscoin.Test.Consensus"
    [ testProperty "Nodes converge with partitions" $
        propNetworkNodesConverge @c @Nakamoto.PoW @(NakamotoNodeState c)
                                 initNetwork
                                 (arbitraryPartitionedNetwork Nakamoto.blockTime)
                                 config
    , testProperty "Nodes converge without partitions" $
        propNetworkNodesConverge @c @Nakamoto.PoW @(NakamotoNodeState c)
                                 initNetwork
                                 (arbitraryHealthyNetwork Nakamoto.blockTime)
                                 config
    ]

-- | Tests whether simulated nodes converge to a single chain.
propNetworkNodesConverge
    :: forall c s a m
     . (IsCrypto c, Serialise s, Ord s, TestableNode c s m a)
    => (TestNetwork c s () -> TestNetwork c s a) -- ^ Network initialization function
    -> Gen (TestNetwork c s ())                  -- ^ TestNetwork generator
    -> Consensus.Config                          -- ^ Static protocol configuration.
    -> Property
propNetworkNodesConverge tnInit genNetworks config =
    forAllShrink genNetworks shrink $ \tn ->
        networkNonTrivial tn ==>
            let tn'             = runNetwork config (tnInit tn)

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
                score           = similarityScore falloff (nodePrefixes tn')

                --
                -- Properties to be tested
                --
                -- Ensure that we have a minimum of chain growth. This means nodes need
                -- to have agreed on blocks beyond the genesis, which all nodes start
                -- with.
                propChainGrowth    = chainLength > 1
                -- Ensure that nodes are replicating client transactions in their blocks.
                -- It's not enough to check that blocks are replicated, since empty
                -- blocks are considered valid.
                propReplication    = not . null $ replicatedTxs

                propLowConvergence   = nodeCount == 1 || score >= 0.45
                propMajConvergence   = nodeCount == 1 || score >= 0.5
                propHighConvergence  = nodeCount == 1 || score >= 0.8
                propSuperConvergence = nodeCount == 1 || score >= 0.9

             in cover 70.0 propReplication      "replicated any data"
              . cover 75.0 propChainGrowth      "have more than one block"
              . cover 70.0 propSuperConvergence "have super convergence"
              . cover 80.0 propHighConvergence  "have high convergence"
              . cover 95.0 propMajConvergence   "have majority convergence"
              . counterexample (T.unpack $ prettyCounterexample tn' replicatedTxs score)
              $ propLowConvergence

-- | Counts how many times each item figures in the list.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies xs = Map.toList $ Map.fromListWith (+) [(x, 1) | x <- xs]

-- | Similarity score is a floating point number between @0@ and @1@ which
-- represents how similar the input prefixes are with regards to each other.
--
-- The function is biased in such a way that *less* partitions score higher
-- than *more* partitions, all else being equal.
--
-- Takes a falloff function which is applied to the columns in the input. This
-- is useful to encode the idea that elements towards the end of the prefixes
-- should weigh less on the final score, than elements at the head.
similarityScore :: Ord a => (Float -> Float) -> [[a]] -> Float
similarityScore f xs =
    sum actual / sum ideal
  where
    -- Distribution of weights by column. Uses the function @f@ to weight
    -- each column by its position in the list.
    dist    = [f (x / fromIntegral ncols) | x <- [0..]]

    actual  = zipWith (*) dist (map uniformity cols)
    ideal   = take ncols dist

    -- Columns and rows
    cols    = transpose xs
    ncols   = length cols
    nrows   = length xs

    -- Measures how uniform a list is.
    --
    -- > uniformity [1,1,1]
    -- 1.0
    --
    -- > uniformity [1,2,3]
    -- 0.33
    --
    uniformity :: Ord a => [a] -> Float
    uniformity col =
        1.0 / fromIntegral (groups + penalty)
      where
        -- The number of distinct groups of values.
        groups  = length (frequencies col)
        -- If rows are missing, this penalty will be greater than zero.
        penalty = nrows - length col

-- | Exponential fall-off function, such that as @x@ reaches @1.0@, @falloff x@
-- reaches @0.0@.
falloff :: Float -> Float
falloff x = -x ** 4 + 1

-- | Return a pretty-printed TestNetwork for counter-examples.
prettyCounterexample
    :: ( Ord s
       , TestableNode c s m a
       , IsCrypto c
       )
    => TestNetwork c s a
    -> [DummyTx]
    -> Float
    -> Text
prettyCounterexample tn@TestNetwork{..} txsReplicated score =
    prettyLog <> prettyInfo <> prettyNodes <> prettyStats
  where
    prettyLog    = T.unlines $  " log:" : prettyMsgs tnLog
    prettyNodes  = T.unlines $ [" nodes:", "  " <> show (length tnNodes)]
    prettyInfo   = T.unlines $ [" info:", T.unlines ["  " <> show id <> ": " <> testableShow n | (id, n) <- Map.toList tnNodes]]
    prettyStats  = T.unlines $ [" msgs sent: "      <> show tnMsgCount,
                                " txs replicated: " <> show (length txsReplicated),
                                " common prefix: "  <> show (length $ longestCommonPrefix $ nodePrefixes tn),
                                " convergence: "    <> show (score * 100) <> "%",
                                " last tick: "      <> prettyDuration (sinceEpoch tnLastTick),
                                " latencies: "      <> T.unwords (map prettyDuration $ take 10 tnLatencies) <> " ...",
                                " scheduled:\n"     <> T.unlines (prettyMsgs (Set.filter (not . isTick) tnScheduled))]
    prettyMsgs ms = reverse ["  " <> prettyScheduled l | l <- reverse $ sort $ toList ms]

longestChain
    :: (TestableNode c s m a)
    => TestNetwork c s a
    -> [BlockHash c]
longestChain tn = maximumBy (comparing length) (nodePrefixes tn)

-- | The longest chain prefixes of all nodes in the network.
nodePrefixes :: (TestableNode c s m a) => TestNetwork c s a -> [[BlockHash c]]
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
