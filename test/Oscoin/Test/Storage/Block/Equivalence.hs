{- | In this module we spawn two different 'BlockStore' implementations, the
   pure and the SQLite one and we check their operations are equivalent, i.e
   both implementations returns the same results.
-}
module Oscoin.Test.Storage.Block.Equivalence
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Types (RadTx)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Block.SQLite as SQLite
import qualified Oscoin.Storage.Block.STM as STM

import           Oscoin.Test.Storage.Block.Generators
import           Oscoin.Test.Storage.Block.SQLite (DummySeal, defaultGenesis)

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testProperty "getTip . insertBlock equivalence"  propInsertGetTipEquivalence
    , testProperty "(forks) getTip . insertBlock equivalence"  propForksInsertGetTipEquivalence
    ]

-- | Classify a 'Blockchain' based on its length.
classifyChain :: Blockchain tx s -> Property -> Property
classifyChain chain =
    let chainLen = length (blocks chain)
    in classify (chainLen <= 10) "small chains  (< 10 blocks)" .
       classify (chainLen <= 50) "medium chains (< 50 blocks)" .
       classify (chainLen > 50)  "long chains   (> 50 blocks)"

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

-- | Check that given a random 'Blockchain', we can insert a bunch of blocks
-- and get the tip, and that both stores agree on the result.
propInsertGetTipEquivalence :: Property
propInsertGetTipEquivalence =
    forAll (resize 25 $ genBlockchainFrom defaultGenesis) $ \chain -> do
        classifyChain chain $
            ioProperty $ withStores $ \stores -> do
                p1 <- apiCheck stores (`Abstract.insertBlocksNaive` (blocks chain))
                p2 <- apiCheck stores Abstract.getTip
                pure (p1 .&&. p2)

-- | In this 'Property', we do the following:
-- 1. We start from a default chain;
-- 2. We generate some random orphan chains together with the \"missing link\"
--    necessary to make them non-orphans;
-- 3. After inserting each orphan chain, we check 'getOrphans' is consistent
--    between implementations;
-- 4. We add the \"missing link\", assessing the tip coincides.
propForksInsertGetTipEquivalence :: Property
propForksInsertGetTipEquivalence = do
    let forkParams = ForkParams 0 10 3  -- 3 forks of max 10 blocks.
        generator = do
            chain <- resize 20 $ genBlockchainFrom defaultGenesis
            orph  <- genOrphanChainsFrom forkParams chain
            pure (chain, orph)
    forAll generator $ \(chain, orphansWithLink) -> do
        ioProperty $ withStores $ \stores -> do
            -- Step 1: Store the chain in both stores.
            _ <- apiCheck stores (`Abstract.insertBlocksNaive` (blocks chain))
            forM_ orphansWithLink $ \(orphans, missingLink) -> do
                -- Step 2: Store the orphan chains
                p1 <- apiCheck stores (`Abstract.insertBlocksNaive` (blocks orphans))
                p2 <- apiCheck stores Abstract.getOrphans
                -- Step 3: Add the missing link and check the tip
                p3 <- apiCheck stores (`Abstract.insertBlocksNaive` [missingLink])
                p4 <- apiCheck stores Abstract.getTip
                pure (p1 .&&. p2 .&&. p3 .&&. p4)

{------------------------------------------------------------------------------
  Useful combinators
------------------------------------------------------------------------------}

type StoresUnderTest tx s m =
    (Abstract.BlockStore tx s m, Abstract.BlockStore tx s m)

-- | Initialises both the SQL and the STM store and pass them to the 'action'.
withStores :: (StoresUnderTest RadTx DummySeal IO -> IO a) -> IO a
withStores action =
    SQLite.withBlockStore ":memory:" defaultGenesis blockScore noValidation $ \sqlStore ->
        STM.withBlockStore (fromGenesis defaultGenesis) blockScore noValidation $ \stmStore ->
            action (sqlStore, stmStore)

-- | When given a function from a 'BlockStore' operation to a result 'a', it
-- calls the function over both stores and returns whether or not the result
-- matches.
apiCheck :: (Monad m, Eq b, Show b)
         => StoresUnderTest tx s m
         -> (Abstract.BlockStore tx s m -> m b)
         -> m Property
apiCheck (store1, store2) apiCall = do
    res1 <- apiCall store1
    res2 <- apiCall store2
    pure (res1 === res2)
