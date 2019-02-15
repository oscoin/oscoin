{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Storage.Block.Orphanage
    ( tests
    ) where

import           Oscoin.Prelude


import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (blockScore)
import           Oscoin.Storage.Block.Orphanage

import           Oscoin.Test.Crypto.Blockchain.Generators
                 (ForkParams(..), genBlockchainFrom, genOrphanChainsFrom)
import           Oscoin.Test.Storage.Block.SQLite
                 ( bestChainOracle
                 , defaultGenesis
                 , fromShuffled
                 , insertOrphans
                 , shuffledOrphanChains
                 )
import           Oscoin.Test.Util (Condensed(..), showOrphans)

import           Test.QuickCheck (property)
import           Test.QuickCheck.Extended ((===))
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding ((===))

tests :: [TestTree]
tests =
    [ testProperty "insert orphans"       propInsertOrphans
    , testProperty "selectBestCandidate (single choice)"    propSelectBestCandidateSingleChoice
    , testProperty "selectBestCandidate (multiple choices)" propSelectBestCandidateMultipleChoice
    , testProperty "selectBestCandidate (long initial chain, multiple choices)" propSelectBestCandidateComplex
    ]

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

propInsertOrphans :: Property
propInsertOrphans = do
    let orphanage    = emptyOrphanage blockScore
        initialChain = unsafeToBlockchain [defaultGenesis]
        showIt       = showOrphans . (initialChain,)
        forkParams   = ForkParams 0 3 1  -- 1 fork of max 3 blocks.
    forAllShow (genOrphanChainsFrom forkParams initialChain) showIt $ \chains ->
        monadic runIdentity $ do
            ps <- forM chains $ \(orphanChain, _missingLink) -> do
                     let actual = toList $ getOrphans (insertOrphans (blocks orphanChain) orphanage)
                     pure (sort actual === sort (map blockHash (blocks orphanChain)))
            pure $ foldl' (.&&.) (property True) ps

-- Generates a bunch of candidates and check the best one is selected.
-- In this test, we limit the selection to only one choice, i.e. at any given
-- time there is /only one/ best chain to select.
propSelectBestCandidateSingleChoice :: Property
propSelectBestCandidateSingleChoice = do
    let orphanage    = emptyOrphanage blockScore
        initialChain = unsafeToBlockchain [defaultGenesis]
        showIt       = showOrphans . (initialChain,)
        forkParams = ForkParams 0 10 10  -- 10 fork of max 10 blocks.
    forAllShow (genOrphanChainsFrom forkParams initialChain) showIt $ \chains ->
        monadic runIdentity $ do
            ps <- forM chains $ \(orphanChain, missingLink) -> do
                -- First of all, we insert all the orphans up to the missing
                -- link, and we assess that the best chain must be 'Nothing',
                -- as we don't have any candidate linking to the main chain.00
                let orphanage' = insertOrphans (blocks orphanChain) orphanage
                let p1 = selectBestChain [blockHash defaultGenesis] orphanage' === Nothing
                -- Now we insert the missing link and we check the chain is
                -- what we expect.
                let orphanage'' = insertOrphans [missingLink] orphanage'
                let bestChain = reverse . toBlocksOldestFirst orphanage'' . snd <$>
                      selectBestChain [blockHash defaultGenesis] orphanage''
                let p2 = bestChain === Just (blocks orphanChain <> [missingLink])
                pure [p1, p2]
            pure $ classifyChainsByScore chains $
                       foldl' (.&&.) (property True) (mconcat ps)

-- Generates a bunch of candidates and check the best one is selected. The
-- orphans blocks are fed shuffled to the 'Orphanage', to test its ability to
-- recognise and fuse chain fragments.
propSelectBestCandidateMultipleChoice :: Property
propSelectBestCandidateMultipleChoice = do
    let orphanage    = emptyOrphanage blockScore
        initialChain = unsafeToBlockchain [defaultGenesis]
        showIt       = showOrphans . (initialChain,) . map (\(_,b,c) -> (b,c))
        forkParams = ForkParams 0 10 10  -- 10 fork of max 10 blocks.
    forAllShow (resize 3 $ shuffledOrphanChains forkParams initialChain) showIt $ \chains ->
            let finalOrphanage =
                  foldl' (\o (shuffledChain, _, missingLink) ->
                            insertOrphans (blocks $ fromShuffled shuffledChain)
                          . insertOrphans [missingLink]
                          $ o -- Order of insertion shouldn't matter!
                        ) orphanage chains
                bestChain = snd <$>
                  selectBestChain [blockHash defaultGenesis] finalOrphanage
            in classifyChainsByScore (map (\(_,b,c) -> (b,c)) chains) $
                 bestChain === bestChainOracle (blockHash defaultGenesis) chains

propSelectBestCandidateComplex :: Property
propSelectBestCandidateComplex = do
    let orphanage = emptyOrphanage blockScore
        generator = do
            chain <- resize 15 $ genBlockchainFrom defaultGenesis
            orph  <- shuffledOrphanChains forkParams chain
            pure (chain, orph)
        showIt (chns, orph) = showOrphans . (chns,) . map (\(_,b,c) -> (b,c)) $ orph
        forkParams = ForkParams 0 10 10  -- 10 fork of max 10 blocks.
    forAllShow generator showIt $ \(_initialChain, chains) ->
            let (finalOrphanage, allLinks) =
                  foldl' (\(o,ls) (shuffledChain, _, missingLink) -> (
                            insertOrphans (blocks $ fromShuffled shuffledChain)
                          . insertOrphans [missingLink]
                          $ o, missingLink : ls)
                        ) (orphanage, mempty) chains
                bestChain lnk = snd <$> selectBestChain [parentHash lnk] finalOrphanage
                props = map (\lnk -> bestChain lnk == bestChainOracle (parentHash lnk) chains) allLinks
            in classifyChainsByScore (map (\(_,b,c) -> (b,c)) chains) $
                foldl' (.&&.) (property True) props


{------------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

classifyChainsByScore :: forall tx s. [(Blockchain tx s, Block tx s)] -> Property -> Property
classifyChainsByScore chains = tabulate "Chain Score" scores
  where
      scores :: [String]
      scores = map (\(c,lnk) -> toScore (lnk : blocks c)) chains

      toScore :: [Block tx s] -> String
      toScore blks = case totalScore blks of
                       x | x == 0     -> "0 score"
                       x | x <= 10    -> "0-10 score"
                       x | x <= 50    -> "10-50 score"
                       x | x <= 500   -> "50-500 score"
                       x | x <= 5000  -> "500-5000 score"
                       x | x <= 50000 -> "5000-50000 score"
                       _              -> ">50000 score"

      totalScore :: [Block tx s] -> Score
      totalScore = sum . map blockScore

{------------------------------------------------------------------------------
  (Temporary) Orphans
------------------------------------------------------------------------------}

instance Show s => Condensed (ChainCandidate s) where
    condensed ChainCandidate{..} =
           "Candidate { chain = "
        <> condensed candidateChain
        <> ", tip  = "   <> show candidateTipHeader
        <> ", score  = " <> condensed candidateScore
        <> " }"
