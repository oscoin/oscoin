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
import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.RadicleTx
import           Oscoin.Environment (Environment(Testing))
import           Oscoin.ProtocolConfig (getProtocolConfig)
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

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit.Extended

tests :: [TestTree]
tests =
    [ testGroup "Storage.Block"
        [ testCase "Store/lookup Block" (withMemDB testStoreLookupBlock)
        , testCase "Store/lookup Tx"    (withMemDB testStoreLookupTx)
        , testCase "Get Genesis Block"  (withMemDB testGetGenesisBlock)
        , testCase "Get blocks"         (withMemDB testGetBlocks)
        , testCase "Get Score"          (withMemDB testGetScore)
        , testCase "Orphans"            (withMemDB testOrphans)
        , testCase "isStored"           (withMemDB testIsStored)
        , testCase "isConflicting"      (withMemDB testIsConflicting)
        ]
    , testGroup "Storage.Block Forks"
        [ testCase "One block fork" (withMemDB testFork1)
        , testCase "Two block fork" (withMemDB testFork2)
        ]
    ]

type DummySeal = Text

defaultGenesis :: Block tx DummySeal
defaultGenesis =
    sealBlock mempty (emptyGenesisBlock Time.epoch)

withMemDB :: (Handle RadTx DummySeal -> IO a) -> IO a
withMemDB action = do
    cfg <- getProtocolConfig Testing -- It doesn't matter as it's not used by 'blockValidate'.
    bracket (open ":memory:" blockScore blockValidate cfg >>= initialize defaultGenesis)
            close
            action
  where
    blockValidate _ _ _ = Right ()

testStoreLookupBlock :: Handle RadTx DummySeal -> Assertion
testStoreLookupBlock h = do
    gen <- getGenesisBlock h
    blk <- linkParent gen <$> generate arbitraryBlock
    storeBlock h blk
    Just blk' <- lookupBlock h (blockHash blk)

    blk' @?= blk

testGetScore :: Handle RadTx DummySeal -> Assertion
testGetScore h = do
    gen <- getGenesisBlock h

    chain :: Blockchain RadTx DummySeal <-
        generate $ (arbitraryValidBlockchainFrom gen
            `suchThat` (\c -> height c > 6))

    storeBlockchain' h (initDef [] $ blocks chain)

    score <- getChainScore (hConn h)
    score @?= sum (map blockScore (blocks chain))

    [blk1, blk2, blk3] <- pure $ takeBlocks 3 chain
    score' <- getChainSuffixScore (hConn h) (blockHash blk3)
    score' @?= blockScore blk1 + blockScore blk2

testFork1 :: Handle RadTx DummySeal -> Assertion
testFork1 h = do
    gen <- getGenesisBlock h

    blk <- generate $ arbitraryValidBlockWith (blockHeader gen) []
    storeBlock h blk

    blk' <- withDifficulty (Difficulty $ blockScore blk + 1)
        <$> generate (arbitraryValidBlockWith (blockHeader gen) [])
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

testFork2 :: Handle RadTx DummySeal -> Assertion
testFork2 h@Handle{..} = do
    gen <- getGenesisBlock h

    blk <- generate $ arbitraryValidBlockWith (blockHeader gen) []
    storeBlock h blk

    -- The first conflicting block we add doesn't have enough score.
    blk1 <- withDifficulty (Difficulty $ blockScore blk - 1)
        <$> generate (arbitraryValidBlockWith (blockHeader gen) [])
    storeBlock h blk1

    -- The second one plus the first do.
    blk2 <- withDifficulty 2
        <$> generate (arbitraryValidBlockWith (blockHeader blk1) [])
    storeBlock h blk2

    -- `blk` is not part of the best chain anymore.
    hashes <- getChainHashes hConn
    hashes @?= [blockHash blk2, blockHash blk1, blockHash gen]

    -- The orphans list has been purged.
    os <- getOrphans h
    os @?= mempty

testStoreLookupTx :: Handle RadTx DummySeal -> Assertion
testStoreLookupTx h = do
    gen <- getGenesisBlock h
    blk <- linkParent gen <$>
        generate (arbitraryBlock `suchThat` (not . null . blockData))

    storeBlock h blk

    let tx = headDef (panic "No transactions!")
           $ toList $ blockData blk

    Just tx' <- lookupTx h (Crypto.hash tx)

    tx' @?= tx

testGetGenesisBlock :: Handle RadTx DummySeal -> Assertion
testGetGenesisBlock h = do
    blk <- getGenesisBlock h
    defaultGenesis @?= blk

testOrphans :: Handle RadTx DummySeal -> Assertion
testOrphans h = do
    blks <- replicateM 10 (generate $ arbitraryBlockWith [])

    for_ blks (storeBlock h)
    blks' <- getOrphans h

    blks' @?= Set.fromList (map blockHash blks)

testGetBlocks :: Handle RadTx DummySeal -> Assertion
testGetBlocks h = do
    gen <- getGenesisBlock h

    chain :: Blockchain RadTx DummySeal <-
        generate $ resize 1 $ arbitraryValidBlockchainFrom gen

    storeBlockchain' h (initDef [] $ blocks chain)

    -- This orphan block shouldn't be returned by 'maximumChainBy'.
    storeBlock h =<< generate arbitraryBlock

    blks' <- getBlocks h (fromIntegral $ height chain)

    blocks chain @?= blks'

testIsStored :: Handle RadTx DummySeal -> Assertion
testIsStored h@Handle{..} = do
    gen <- getGenesisBlock h

    result <- isStored hConn (blockHash gen)
    result @?= True

    result' <- isStored hConn Crypto.zeroHash
    result' @?= False

testIsConflicting :: Handle RadTx DummySeal -> Assertion
testIsConflicting h@Handle{..} = do
    gen <- getGenesisBlock h

    blk <- linkParent gen <$> generate arbitraryBlock
    storeBlock h blk

    blk' <- linkParent gen <$> generate arbitraryBlock
    result <- isConflicting hConn blk'
    result @?= True

    result' <- isConflicting hConn (linkParent blk blk')
    result' @?= False

-- Helpers ---------------------------------------------------------------------

withDifficulty :: Serialise s => Difficulty -> Block tx s -> Block tx s
withDifficulty d blk =
    blk { blockHeader = header, blockHash = headerHash header }
  where
    header = (blockHeader blk) { blockTargetDifficulty = d }
