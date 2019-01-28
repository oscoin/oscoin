module Oscoin.Test.Storage.Block
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
                 ( Blockchain(..)
                 , blocks
                 , emptyGenesisBlock
                 , height
                 , takeBlocks
                 )
import           Oscoin.Crypto.Blockchain.Block hiding (genesisBlock)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.RadicleTx
import           Oscoin.Storage.Block.SQLite
import           Oscoin.Storage.Block.SQLite.Internal
import qualified Oscoin.Time as Time

import           Oscoin.Test.Crypto.Blockchain.Arbitrary
                 ( arbitraryBlock
                 , arbitraryBlockWith
                 , arbitraryValidBlockWith
                 , arbitraryValidBlockchainFrom
                 )
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()

import           Codec.Serialise (Serialise)
import qualified Data.Set as Set

import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testGroup "Storage.Block"
        [ testProperty "Store/lookup Block" (withMemDB genGenesisLinkedBlock testStoreLookupBlock)
        , testProperty "Store/lookup Tx"    (withMemDB genNonEmptyBlock testStoreLookupTx)
        , testProperty "Get Genesis Block"  (withMemDB (const arbitrary) testGetGenesisBlock)
        , testProperty "Get blocks"         (withMemDB genGetBlocks testGetBlocks)
        , testProperty "Get Score"          (withMemDB genAtLeastSixBlocks testGetScore)
        , testProperty "Orphans"            (withMemDB (const (vectorOf 10 (arbitraryBlockWith []))) testOrphans)
        , testProperty "isStored"           (withMemDB (const arbitrary) testIsStored)
        , testProperty "isConflicting"      (withMemDB (\g -> (,) <$> genGenesisLinkedBlock g
                                                                  <*> genGenesisLinkedBlock g
                                                       ) testIsConflicting)
        ]
    , testGroup "Storage.Block Forks"
        [ testProperty "One block fork" (withMemDB genTestFork1 testFork1)
        , testProperty "Two block fork" (withMemDB genTestFork2 testFork2)
        ]
    ]

type DummySeal = Text

defaultGenesis :: Block tx DummySeal
defaultGenesis =
    sealBlock mempty (emptyGenesisBlock Time.epoch)

{------------------------------------------------------------------------------
  Generators tailored for the tests at hand
------------------------------------------------------------------------------}

-- | Generates an arbitrary 'Block' which is linked with the genesis block
-- and that has a valid timestamp (> timestamp(genesis)).
genGenesisLinkedBlock :: Block RadTx DummySeal -> Gen (Block RadTx DummySeal)
genGenesisLinkedBlock genesisBlock =
    let genesisTimestamp = blockTimestamp . blockHeader $ genesisBlock
        blockInTheFuture = arbitraryBlock
            `suchThat` ((> genesisTimestamp) . blockTimestamp . blockHeader)
    in linkParent genesisBlock <$> blockInTheFuture

-- | Generates a non-empty 'Block'.
genNonEmptyBlock :: Block RadTx DummySeal -> Gen (Block RadTx DummySeal)
genNonEmptyBlock genesisBlock =
    genGenesisLinkedBlock genesisBlock `suchThat` (not . null . blockData)

-- | Generates a 'Blockchain' with at least six blocks.
genAtLeastSixBlocks :: Block RadTx DummySeal -> Gen (Blockchain RadTx DummySeal)
genAtLeastSixBlocks genesisBlock =
    arbitraryValidBlockchainFrom genesisBlock `suchThat` (\c -> height c > 6)

-- | Generates two 'Block's, one which has difficulty greater than the second.
genTestFork1 :: Block RadTx DummySeal
             -> Gen (Block RadTx DummySeal, Block RadTx DummySeal)
genTestFork1 genesisBlock = do
    blk  <- arbitraryValidBlockWith (blockHeader genesisBlock) []
    blk' <- withDifficulty (Difficulty $ blockScore blk + 1)
        <$> arbitraryValidBlockWith (blockHeader genesisBlock) []
    pure (blk, blk')

-- | Generates three 'Block's, in increasing order of 'Difficulty'.
genTestFork2 :: Block RadTx DummySeal
             -> Gen (Block RadTx DummySeal, Block RadTx DummySeal, Block RadTx DummySeal)
genTestFork2 genesisBlock = do
    blk  <- arbitraryValidBlockWith (blockHeader genesisBlock) []
    -- The first conflicting block we add doesn't have enough score.
    blk1 <- withDifficulty (Difficulty $ blockScore blk - 1)
        <$> arbitraryValidBlockWith (blockHeader genesisBlock) []
    -- The second one plus the first do.
    blk2 <- withDifficulty 2
        <$> arbitraryValidBlockWith (blockHeader blk1) []
    pure (blk, blk1, blk2)

genGetBlocks :: Block RadTx DummySeal
             -> Gen (Block RadTx DummySeal, Blockchain RadTx DummySeal)
genGetBlocks genesisBlock =
    (,) <$> arbitraryBlock
        <*> resize 1 (arbitraryValidBlockchainFrom genesisBlock)


{------------------------------------------------------------------------------
  The tests proper
------------------------------------------------------------------------------}

testStoreLookupBlock :: Block RadTx DummySeal
                     -> Handle RadTx DummySeal
                     -> Assertion
testStoreLookupBlock blk h = do
    storeBlock h blk
    Just blk' <- lookupBlock h (blockHash blk)

    blk' @?= blk

testGetScore :: Blockchain RadTx DummySeal -> Handle RadTx DummySeal -> Assertion
testGetScore chain h = do

    storeBlockchain' h (initDef [] $ blocks chain)

    score <- getChainScore (hConn h)
    score @?= sum (map blockScore (blocks chain))

    [blk1, blk2, blk3] <- pure $ takeBlocks 3 chain
    score' <- getChainSuffixScore (hConn h) (blockHash blk3)
    score' @?= blockScore blk1 + blockScore blk2

testFork1 :: (Block RadTx DummySeal, Block RadTx DummySeal)
          -> Handle RadTx DummySeal
          -> Assertion
testFork1 (blk, blk') h = do
    gen <- getGenesisBlock h

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
          -> Handle RadTx DummySeal
          -> Assertion
testFork2 (blk, blk1, blk2) h@Handle{..} = do
    gen <- getGenesisBlock h

    storeBlock h blk
    storeBlock h blk1
    storeBlock h blk2

    -- `blk` is not part of the best chain anymore.
    hashes <- getChainHashes hConn
    hashes @?= [blockHash blk2, blockHash blk1, blockHash gen]

    -- The orphans list has been purged.
    os <- getOrphans h
    os @?= mempty

testStoreLookupTx :: Block RadTx DummySeal
                  -> Handle RadTx DummySeal
                  -> Assertion
testStoreLookupTx blk h = do
    storeBlock h blk

    let tx = headDef (panic "No transactions!")
           $ toList $ blockData blk

    Just tx' <- lookupTx h (Crypto.hash tx)

    tx' @?= tx

testGetGenesisBlock :: () -> Handle RadTx DummySeal -> Assertion
testGetGenesisBlock () h = do
    blk <- getGenesisBlock h
    defaultGenesis @?= blk

testOrphans :: [Block RadTx DummySeal]
            -> Handle RadTx DummySeal
            -> Assertion
testOrphans blks h = do
    for_ blks (storeBlock h)
    blks' <- getOrphans h

    blks' @?= Set.fromList (map blockHash blks)

testGetBlocks :: (Block RadTx DummySeal, Blockchain RadTx DummySeal)
              -> Handle RadTx DummySeal
              -> Assertion
testGetBlocks (block, chain) h = do
    storeBlockchain' h (initDef [] $ blocks chain)

    -- This orphan block shouldn't be returned by 'maximumChainBy'.
    storeBlock h block

    blks' <- getBlocks h (fromIntegral $ height chain)

    blocks chain @?= blks'

testIsStored :: () -> Handle RadTx DummySeal -> Assertion
testIsStored () h@Handle{..} = do
    gen <- getGenesisBlock h

    result <- isStored hConn (blockHash gen)
    result @?= True

    result' <- isStored hConn Crypto.zeroHash
    result' @?= False

testIsConflicting :: (Block RadTx DummySeal, Block RadTx DummySeal)
                  -> Handle RadTx DummySeal
                  -> Assertion
testIsConflicting (blk, blk') h@Handle{..} = do
    storeBlock h blk

    result <- isConflicting hConn blk'
    result @?= True

    result' <- isConflicting hConn (linkParent blk blk')
    result' @?= False

{------------------------------------------------------------------------------
  Useful combinators & helpers
------------------------------------------------------------------------------}

withMemDB :: Show a
          => (Block RadTx DummySeal -> Gen a)
          -> (a -> Handle RadTx DummySeal -> IO ())
          -> Property
withMemDB genTestData action = once $ monadicIO $ do
    testData <- pick (genTestData defaultGenesis)
    liftIO $
        bracket (open ":memory:" blockScore blockValidate >>= initialize defaultGenesis)
                close
                (action testData)
  where
    blockValidate _ _ = Right ()

withDifficulty :: Serialise s => Difficulty -> Block tx s -> Block tx s
withDifficulty d blk =
    blk { blockHeader = header, blockHash = headerHash header }
  where
    header = (blockHeader blk) { blockTargetDifficulty = d }

