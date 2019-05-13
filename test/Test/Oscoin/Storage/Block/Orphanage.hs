{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Oscoin.Storage.Block.Orphanage
    ( tests
    ) where

import           Oscoin.Prelude hiding (reverse)
import qualified Prelude (last, reverse)


import           Oscoin.Consensus.Nakamoto (blockScore)
import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Blockchain as Blockchain
import           Oscoin.Storage.Block.Orphanage
import qualified Oscoin.Time as Time
import           Oscoin.Time.Chrono (reverse, toNewestFirst)

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Helpers
                 (defaultBeneficiary)
import           Oscoin.Test.Crypto.Blockchain.Generators
import           Oscoin.Test.Util (Condensed(..))

import           Data.ByteArray.Orphans ()
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
    :: IsCrypto c
    => [Block c tx (Sealed c s)]
    -> Orphanage c tx s
    -> Orphanage c tx s
insertOrphans xs o =
    foldl' (flip insertOrphan) o xs


defaultGenesis :: IsCrypto c => Block c () (Sealed c ())
defaultGenesis = sealBlock mempty (emptyGenesisBlock Time.epoch defaultBeneficiary)

classifyChainsByScore
    :: forall c tx s. [(Blockchain c tx s, Block c tx (Sealed c s))]
    -> Property
    -> Property
classifyChainsByScore chains = tabulate "Chain Score" scores
  where
      scores :: [String]
      scores = map (\(c,lnk) -> toScore (lnk : toNewestFirst (blocks c))) chains

      toScore :: [Block c tx (Sealed c s)] -> String
      toScore blks = case totalScore blks of
                       x | x == 0     -> "0 score"
                       x | x <= 10    -> "0-10 score"
                       x | x <= 50    -> "10-50 score"
                       x | x <= 500   -> "50-500 score"
                       x | x <= 5000  -> "500-5000 score"
                       x | x <= 50000 -> "5000-50000 score"
                       _              -> ">50000 score"

      totalScore :: [Block c tx (Sealed c s)] -> Score
      totalScore = sum . map blockScore

bestChainOracle
    :: IsCrypto c
    => BlockHash c
    -- ^ The hash of the adopted block this chain originates from.
    -> [(Blockchain c tx s, Block c tx (Sealed c s))]
    -> Maybe (ChainCandidate c s)
bestChainOracle _ [] = Nothing
bestChainOracle root xs =
    let (chain, lnk) =
            maximumBy (\(chain1,lnk1) (chain2, lnk2) ->
                sum (map blockScore (lnk1 : (toNewestFirst . blocks) chain1)) `compare`
                sum (map blockScore (lnk2 : (toNewestFirst . blocks) chain2))
            ) (filter (\(_,ls) -> parentHash ls == root) xs)
        winningChain = Prelude.reverse ((toNewestFirst . blocks) chain <> [lnk])
    in Just $ ChainCandidate
        { candidateChain     = Seq.fromList (map blockHash winningChain)
        , candidateTipHeader = blockHeader (Prelude.last winningChain)
        , candidateScore     = sum (map blockScore winningChain)
        }


{------------------------------------------------------------------------------
  (Temporary) Orphans
------------------------------------------------------------------------------}

instance (IsCrypto c, Show s) => Condensed (ChainCandidate c s) where
    condensed ChainCandidate{..} =
           "Candidate { chain = "
        <> condensed candidateChain
        <> ", tip  = "   <> show candidateTipHeader
        <> ", score  = " <> condensed candidateScore
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
