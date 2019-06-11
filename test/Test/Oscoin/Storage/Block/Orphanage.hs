{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Oscoin.Storage.Block.Orphanage
    ( tests
    ) where

import           Oscoin.Prelude hiding (reverse)
import qualified Prelude (head, reverse)


import           Oscoin.Consensus.Nakamoto (blockScore)
import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Blockchain as Blockchain
import           Oscoin.Storage.Block.Orphanage
import           Oscoin.Time.Chrono (OldestFirst(..), reverse, toNewestFirst)

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Blockchain.Generators
import           Oscoin.Test.Util (Condensed(..), condensedS)

import           Data.ByteArray.Orphans ()
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T

import           Test.QuickCheck (property)
import           Test.QuickCheck.Extended ((===))
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding ((===))

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Storage.Block.Orphanage"
    [ testProperty "propInsertOrphansGetOrphans" (propInsertOrphansGetOrphans d)
    , testProperty "propSelectBestCandidateSingleChoice" (propSelectBestCandidateSingleChoice d)
    , testProperty "propSelectBestCandidateMultipleChoice" (propSelectBestCandidateMultipleChoice d)
    , testProperty "propSelectBestCandidateComplex" (propSelectBestCandidateComplex d)
    , testProperty "propFuseChains"  (propFuseChains d)
    , testProperty "propPruneChainsShallow" (propPruneChainsShallow d)
    , testProperty "propPruneChainsDeep"    (propPruneChainsDeep d)
    ]

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

propInsertOrphansGetOrphans :: forall c. Dict (IsCrypto c) -> Property
propInsertOrphansGetOrphans Dict =
    forAllShow (genOrphanChainsFrom forkParams initialChain) showIt $ \chains ->
        conjoin $ flip map chains $ \(orphanChain, _missingLink) ->
            let blks = toNewestFirst $ blocks orphanChain
                orphanage = insertOrphans blks (emptyOrphanage @c blockScore)
            in Set.fromList (map blockHash blks) === getOrphans orphanage
  where
    initialChain = Blockchain.fromGenesis defaultGenesis
    showIt       = showOrphans . (initialChain,)
    forkParams   = ForkParams 0 3 1  -- 1 fork of max 3 blocks.

-- Generates a bunch of candidates and check the best one is selected.
-- In this test, we limit the selection to only one choice, i.e. at any given
-- time there is /only one/ best chain to select.
propSelectBestCandidateSingleChoice :: forall c. Dict (IsCrypto c) -> Property
propSelectBestCandidateSingleChoice Dict = do
    let orphanage    = emptyOrphanage @c blockScore
        initialChain = Blockchain.fromGenesis defaultGenesis
        showIt       = showOrphans . (initialChain,)
        forkParams = ForkParams 0 10 10  -- 10 fork of max 10 blocks.
    forAllShow (genOrphanChainsFrom forkParams initialChain) showIt $ \chains ->
            classifyChainsByScore chains
                $ conjoin
                $ flip map chains $ \(orphanChain, missingLink) ->
                    -- First of all, we insert all the orphans up to the missing
                    -- link, and we assess that the best chain must be 'Nothing',
                    -- as we don't have any candidate linking to the main chain.00
                    let orphanage' = insertOrphans (toNewestFirst $ blocks orphanChain) orphanage
                        p1 = selectBestChain [blockHash defaultGenesis] orphanage' === Nothing
                        -- Now we insert the missing link and we check the chain is
                        -- what we expect.
                        orphanage'' = insertOrphans [missingLink] orphanage'
                        bestChain = toList
                                  . toNewestFirst
                                  . reverse
                                  . toBlocksOldestFirst orphanage''
                                  . snd <$> selectBestChain [blockHash defaultGenesis] orphanage''
                        p2 = bestChain === Just ((toNewestFirst . blocks) orphanChain <> [missingLink])
                    in p1 .&&. p2

-- Generates a bunch of candidates and check the best one is selected. The
-- orphans blocks are fed shuffled to the 'Orphanage', to test its ability to
-- recognise and fuse chain fragments.
propSelectBestCandidateMultipleChoice :: forall c. Dict (IsCrypto c) -> Property
propSelectBestCandidateMultipleChoice Dict = property $ do
    let initialChain = Blockchain.fromGenesis (defaultGenesis @c)
        baseBlockHash = blockHash defaultGenesis
        forkParams = ForkParams 0 10 10  -- 10 fork of max 10 blocks.
    chains <- resize 3 $ genOrphanChainsFrom forkParams initialChain
    orphanage <-
        foldM (\o (chain, missingLink) ->
            insertOrphansShuffled (missingLink : toNewestFirst (blocks chain)) o
            ) (emptyOrphanage blockScore) chains

    let bestChainOrphanage = snd <$>
          selectBestChain [baseBlockHash] orphanage
    pure
        $ counterexample (showOrphans (initialChain, chains))
        $ classifyChainsByScore chains
        $ bestChainOrphanage === bestChainOracle baseBlockHash chains


propSelectBestCandidateComplex :: forall c. Dict (IsCrypto c) -> Property
propSelectBestCandidateComplex Dict = property $ do
    initialChain <- genBlockchainFrom (defaultGenesis @c)
    let forkParams = ForkParams 0 10 10  -- 10 fork of max 10 blocks.
    chains <- genOrphanChainsFrom forkParams initialChain
    orphanage <-
        foldM (\o (chain, missingLink) ->
            insertOrphansShuffled (missingLink : toNewestFirst (blocks chain)) o
            ) (emptyOrphanage blockScore) chains
    let bestChainOrphanage lnk = snd <$> selectBestChain [parentHash lnk] orphanage
    pure
        $ counterexample (showOrphans (initialChain, chains))
        $ classifyChainsByScore chains
        $ conjoin
        $ map (\(_, lnk) -> bestChainOrphanage lnk === bestChainOracle (parentHash lnk) chains) chains

-- | In this property we test that the Orphanage is capable of joining chains
-- together if a \"missig link\" is added as orphan. For example, consider
-- the following chains:
--
-- 0 <- 1 <- 2
-- 4 <- 5
--
-- Now, if 3 comes in, the orphanage should be able to fuse both chains
-- together, resulting into:
--
-- 0 <- 1 <- 2 <- 3 <- 4 <- 5
--
propFuseChains :: forall c. Dict (IsCrypto c) -> Property
propFuseChains Dict = property $ do
    let gen = do
          root       <- genBlockFrom (defaultGenesis @c)
          fusedChain <- toOldestFirst . reverse . blocks
                    <$> genBlockchainFrom root `suchThat` (\c -> chainLength c >= 3)
          -- Generate a random split, making sure to leave at least two blocks
          -- at the end for the singleBlock and the rest of the chain.
          splitIdx   <- choose (1, length fusedChain - 2)

          let (chain1, chain2) = List.splitAt splitIdx fusedChain
          pure (chain1, Prelude.head chain2, drop 1 chain2)

    forAll gen $ \(chain1, singleBlock, chain2) -> do

        let fusedChain = chain1 <> [singleBlock] <> chain2

        let o'' = emptyOrphanage blockScore
                & insertOrphans chain1
                & insertOrphans chain2
        let finalOrphanage = insertOrphan singleBlock o''
        let rootHash = blockHash $ defaultGenesis @c

        let best = do
              (_, c) <- selectBestChain [rootHash] finalOrphanage
              pure . NonEmpty.toList
                   . toOldestFirst
                   . toBlocksOldestFirst finalOrphanage
                   $ c

        let totalSize orph =
                sizeAt rootHash orph + sizeAt (blockHash $ singleBlock) orph

        classifyChainsByLength [unsafeToBlockchain fusedChain] $
            conjoin [ totalSize o'' === 2 -- Before fusing things, there are 2 chains.
                    , best          === Just fusedChain
                    , totalSize finalOrphanage === 1 -- After, only 1.
                    , Map.size (tips finalOrphanage) === 1
                    ]

-- | Tests that 'pruneOrphanageShallow' works as advertised.
propPruneChainsShallow :: forall c. Dict (IsCrypto c) -> Property
propPruneChainsShallow Dict = property $ do
    let gen = do
          commonRoot <- genBlockFrom (defaultGenesis @c)

          -- Two uncorrelated forks all branching from the same block.
          -- Deleting one shouldn't affect the other. @chain1@ has a greater score, so
          -- that it's picked up first.
          chain2      <- genBlockchainFrom commonRoot
          chain1      <- genBlockchainFrom commonRoot
                           `suchThat` (\c -> totalScore c > totalScore chain2)
          pure (chain1, chain2)
    forAllShow gen (uncurry showChains) $ \(chain1, chain2) -> do

        let o'' = emptyOrphanage blockScore
                & insertOrphans (blocks chain1)
                & insertOrphans (blocks chain2)
        let rootHash = blockHash $ defaultGenesis @c
        let firstBestCandidate = snd <$> selectBestChain [rootHash] o''

        let finalOrphanage = do
              c <- firstBestCandidate
              pure $ pruneOrphanageShallow c o''

        let secondBestCandidate = do
              o <- finalOrphanage
              (_, c) <- selectBestChain [rootHash] o
              pure . reverse
                   . toBlocksOldestFirst o
                   $ c

        conjoin [ map (reverse . toBlocksOldestFirst o'') firstBestCandidate
                                                       === Just (blocks' chain1)
                , map (sizeAt rootHash) finalOrphanage === Just 1
                , secondBestCandidate                  === Just (blocks' chain2)
                , map (map fst . Map.toList . tips) finalOrphanage === Just [blockHash (tip chain2)]
                ]

showChains
    :: HasHashing c
    => Blockchain c tx s
    -> Blockchain c tx s
    -> String
showChains chain1 chain2 =
    condensedS chain1 <> " , score: " <> show (totalScore chain1) <> "\n" <>
    condensedS chain2 <> " , score:"  <> show (totalScore chain2)

-- | Tests that 'pruneOrphanageDeep' works as advertised. The code is very
-- similar to 'propPruneChainsShallow', with the difference that now we will
-- also have an orphan branching off the tip of 'chain1'. If we request a
-- deep pruning, such second orphan shouldn't survive.
propPruneChainsDeep :: forall c. Dict (IsCrypto c) -> Property
propPruneChainsDeep Dict = property $ do
    let gen = do
          commonRoot <- genBlockFrom (defaultGenesis @c)

          -- Two uncorrelated forks all branching from the same block.
          -- Deleting one shouldn't affect the other. @chain1@ has a greater score, so
          -- that it's picked up first.
          chain2      <- genBlockchainFrom commonRoot
          chain1      <- genBlockchainFrom commonRoot
                           `suchThat` (\c -> totalScore c > totalScore chain2)
          branchingFork <- genBlockchainFrom (tip chain1)
          pure (chain1, branchingFork, chain2)
    forAllShow gen (\(c1,f1,c2) -> showChains c1 c2 ++ "\n" ++ condensedS f1) $ \(chain1, branchingFork, chain2) -> do

        let o'  = emptyOrphanage blockScore
                & insertOrphans (blocks chain1)
                & insertOrphans (blocks chain2)
                & insertOrphans (blocks branchingFork)
        let rootHash = blockHash $ defaultGenesis @c

        let finalOrphanage = do
              c <- snd <$> selectBestChain [rootHash] o'
              pure $ pruneOrphanageDeep c o'

        let secondBestCandidate = do
              o <- finalOrphanage
              (_, c) <- selectBestChain [rootHash] o
              pure . reverse
                   . toBlocksOldestFirst o
                   $ c

        let chain1TipParent = blockPrevHash . blockHeader $ tip chain1

        -- Test that there should be no trace of candidates once we pruned
        -- chain1 deeply.
        conjoin [ map (sizeAt rootHash) finalOrphanage             === Just 1
                , map (sizeAt chain1TipParent) finalOrphanage      === Just 0
                , map (map fst . Map.toList . tips) finalOrphanage === Just [blockHash (tip chain2)]
                , secondBestCandidate                              === Just (blocks' chain2)
                ]

{------------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

insertOrphansShuffled
    :: IsCrypto c
    => [Block c tx (Sealed c s)]
    -> Orphanage c tx s
    -> Gen (Orphanage c tx s)
insertOrphansShuffled blks o = do
    shuffledBlocks <- shuffle blks
    pure $ insertOrphans shuffledBlocks o

-- | Inserts all the given blocks in the 'Orphanage'.
insertOrphans
    :: Foldable f
    => IsCrypto c
    => f (Block c tx (Sealed c s))
    -> Orphanage c tx s
    -> Orphanage c tx s
insertOrphans xs o =
    foldl' (flip insertOrphan) o xs

defaultGenesis :: IsCrypto c => Block c () (Sealed c ())
defaultGenesis = someGenesisBlock ()

totalScore :: Blockchain c tx s -> Score
totalScore = sum . map blockScore . toNewestFirst . blocks

classifyChainsByScore
    :: forall c tx s. [(Blockchain c tx s, Block c tx (Sealed c s))]
    -> Property
    -> Property
classifyChainsByScore chains = tabulate "Chain Score" scores
  where
      scores :: [String]
      scores = map (\(c,lnk) -> toScore (lnk : toNewestFirst (blocks c))) chains

      toScore :: [Block c tx (Sealed c s)] -> String
      toScore blks = case totalScore (unsafeToBlockchain blks) of
                       x | x == 0     -> "0 score"
                       x | x <= 10    -> "0-10 score"
                       x | x <= 50    -> "10-50 score"
                       x | x <= 500   -> "50-500 score"
                       x | x <= 5000  -> "500-5000 score"
                       x | x <= 50000 -> "5000-50000 score"
                       _              -> ">50000 score"

classifyChainsByLength
    :: forall c tx s. [Blockchain c tx s]
    -> Property
    -> Property
classifyChainsByLength = tabulate "Chain Length" . map toLength
  where
      toLength :: Blockchain c tx s -> String
      toLength chain = case chainLength chain of
                       x | x == 0     -> "0 length"
                       x | x <= 10    -> "0-10 length"
                       x | x <= 50    -> "10-50 length"
                       x | x <= 500   -> "50-500 length"
                       x | x <= 5000  -> "500-5000 length"
                       x | x <= 50000 -> "5000-50000 length"
                       _              -> ">50000 length"

bestChainOracle
    :: IsCrypto c
    => BlockHash c
    -- ^ The hash of the adopted block this chain originates from.
    -> [(Blockchain c tx s, Block c tx (Sealed c s))]
    -> Maybe (ChainCandidate c s)
bestChainOracle _ [] = Nothing
bestChainOracle root xs = do
    (chain, lnk) <- do
        let filtered = filter (\(_,ls) -> parentHash ls == root) xs
        guard (length filtered > 0)
        pure $ maximumBy (\(chain1,lnk1) (chain2, lnk2) ->
            sum (map blockScore (lnk1 : (toNewestFirst . blocks) chain1)) `compare`
            sum (map blockScore (lnk2 : (toNewestFirst . blocks) chain2))
             ) filtered
    let winningChain = Prelude.reverse ((toNewestFirst . blocks) chain <> [lnk])
    pure ChainCandidate
     { candidateChain     = OldestFirst $ Seq.fromList (map blockHash winningChain)
     , candidateScore     = sum (map blockScore winningChain)
     , candidateRootHash  = root
     }

{------------------------------------------------------------------------------
  (Temporary) Orphans
------------------------------------------------------------------------------}

instance IsCrypto c => Condensed (ChainCandidate c s) where
    condensed ChainCandidate{..} =
           "Candidate { chain = "
        <> condensed candidateChain
        <> ", score  = " <> condensed candidateScore
        <> ",rootHash = " <> condensed candidateRootHash
        <> " }"

showOrphans
    :: HasHashing c
    => ( Blockchain c tx s
       , [(Blockchain c tx s, Block c tx (Sealed c s))]
       )
    -> String
showOrphans (initialChain, orphansWithLinks) =
    "chain: "      <> T.unpack (showChainDigest initialChain) <> "\n" <>
    "orphans:\n- " <> T.unpack showOrphanAndLinks
  where
      showOrphanAndLinks =
          T.intercalate "- " . map (\(c,l) -> showChainDigest c <> " link: " <> showBlockDigest l <> "\n")
                             $ orphansWithLinks
