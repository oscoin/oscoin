{- | Whitebox testing for the SQLite storage backend by using
     explicitly its internal functions
-}
module Oscoin.Test.Storage.Block.SQLite.Whitebox
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
                 (Blockchain(..), blocks, height, takeBlocks)
import           Oscoin.Crypto.Blockchain.Block hiding (genesisBlock)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.RadicleTx
import           Oscoin.Storage.Block.SQLite.Internal as Sqlite

import           Oscoin.Test.Crypto.Blockchain.Arbitrary
                 (arbitraryValidBlockWith, arbitraryValidBlockchainFrom)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.Storage.Block.SQLite

import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testGroup "Storage.Block.SQLite whitebox testing (internals)"
        [ testProperty "Get Score"          (withSqliteDB genAtLeastSixBlocks testGetScore)
        , testProperty "isStored"           (withSqliteDB (const arbitrary) testIsStored)
        , testProperty "isConflicting"      (withSqliteDB (\g -> (,) <$> genGenesisLinkedBlock g
                                                                     <*> genGenesisLinkedBlock g
                                                          ) testIsConflicting)
        ]
    , testGroup "Storage.Block.SQLite whitebox (forks)"
        [ testProperty "One block fork" (withSqliteDB genTestFork1 testFork1)
        , testProperty "Two block fork" (withSqliteDB genTestFork2 testFork2)
        ]
    ]

{------------------------------------------------------------------------------
  Generators tailored for the tests at hand
------------------------------------------------------------------------------}

-- | Generates two 'Block's, one which has difficulty greater than the second.
genTestFork1 :: Block RadTx DummySeal
             -> Gen (Block RadTx DummySeal, Block RadTx DummySeal)
genTestFork1 genesisBlock = do
    blk  <- arbitraryValidBlockWith (blockHeader genesisBlock) []
    blk' <- withDifficulty (encodeDifficulty $ blockScore blk + 1)
        <$> arbitraryValidBlockWith (blockHeader genesisBlock) []
    pure (blk, blk')

-- | Generates three 'Block's, in increasing order of 'Difficulty'.
genTestFork2 :: Block RadTx DummySeal
             -> Gen (Block RadTx DummySeal, Block RadTx DummySeal, Block RadTx DummySeal)
genTestFork2 genesisBlock = do
    blk  <- withDifficulty (encodeDifficulty 3)
        <$> arbitraryValidBlockWith (blockHeader genesisBlock) []
    -- The first conflicting block we add doesn't have enough score.
    blk1 <- withDifficulty (encodeDifficulty 2)
        <$> arbitraryValidBlockWith (blockHeader genesisBlock) []
    -- The second one plus the first do.
    blk2 <- withDifficulty (encodeDifficulty 2)
        <$> arbitraryValidBlockWith (blockHeader blk1) []
    pure (blk, blk1, blk2)

-- | Generates a 'Blockchain' with at least six blocks.
genAtLeastSixBlocks :: Block RadTx DummySeal -> Gen (Blockchain RadTx DummySeal)
genAtLeastSixBlocks genesisBlock =
    arbitraryValidBlockchainFrom genesisBlock `suchThat` (\c -> height c > 6)

{------------------------------------------------------------------------------
  The tests proper
------------------------------------------------------------------------------}

testGetScore :: Blockchain RadTx DummySeal
             -> Sqlite.Handle RadTx DummySeal
             -> Assertion
testGetScore chain h = do
    traverse_ (storeBlock h) (reverse $ initDef [] $ blocks chain)

    score <- getChainScore (hConn h)
    score @?= sum (map blockScore (blocks chain))

    [blk1, blk2, blk3] <- pure $ takeBlocks 3 chain
    score' <- getChainSuffixScore (hConn h) (blockHash blk3)
    score' @?= blockScore blk1 + blockScore blk2

testIsStored :: () -> Sqlite.Handle RadTx DummySeal -> Assertion
testIsStored () h = do
    (gen :: Block RadTx DummySeal) <- getGenesisBlock (hConn h)

    result <- isStored (hConn h) (blockHash gen)
    result @?= True

    result' <- isStored (hConn h) Crypto.zeroHash
    result' @?= False

testIsConflicting :: (Block RadTx DummySeal, Block RadTx DummySeal)
                  -> Sqlite.Handle RadTx DummySeal
                  -> Assertion
testIsConflicting (blk, blk') h@Handle{..} = do
    storeBlock h blk

    result <- isConflicting hConn blk'
    result @?= True

    result' <- isConflicting hConn (linkParent blk blk')
    result' @?= False

testFork1 :: (Block RadTx DummySeal, Block RadTx DummySeal)
          -> Sqlite.Handle RadTx DummySeal
          -> Assertion
testFork1 (blk, blk') h = do
    (gen :: Block RadTx DummySeal) <- getGenesisBlock (hConn h)

    storeBlock h blk
    storeBlock h blk'

    hashes <- getChainHashes (hConn h)
    hashes @?= [blockHash blk', blockHash gen]

    -- The chain score shouldn't include the first block we added, since it was
    -- replaced by the second.
    score <- getChainScore (hConn h)
    score @?= blockScore gen + blockScore blk'

    -- The orphans list has been purged.
    os <- getOrphans h
    os @?= mempty

testFork2 :: (Block RadTx DummySeal, Block RadTx DummySeal, Block RadTx DummySeal)
          -> Sqlite.Handle RadTx DummySeal
          -> Assertion
testFork2 (blk, blk1, blk2) h@Sqlite.Handle{..} = do
    (gen :: Block RadTx DummySeal) <- getGenesisBlock hConn

    storeBlock h blk
    storeBlock h blk1
    storeBlock h blk2

    -- `blk` is not part of the best chain anymore.
    hashes <- getChainHashes hConn
    hashes @?= [blockHash blk2, blockHash blk1, blockHash gen]

    -- The orphans list has been purged.
    os <- getOrphans h
    os @?= mempty


