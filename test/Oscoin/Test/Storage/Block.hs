module Oscoin.Test.Storage.Block
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain(..), emptyGenesisBlock)
import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.RadicleTx
import           Oscoin.Storage.Block.SQLite
import qualified Oscoin.Time as Time

import           Oscoin.Test.Crypto.Blockchain.Arbitrary
                 (arbitraryBlock, arbitraryValidBlockchainFrom)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()

import qualified Data.List.NonEmpty as NonEmpty

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit.Extended

tests :: [TestTree]
tests =
    [ testGroup "Storage.Block"
        [ testCase "Store/lookup Block" (runAndRollback testStoreLookupBlock)
        , testCase "Store/lookup Tx"    (runAndRollback testStoreLookupTx)
        , testCase "Get Genesis Block"  (runAndRollback testGetGenesisBlock)
        , testCase "Maximum chain"      (runAndRollback testMaximumChain)
        , expectFail $
            testCase "Orphans"          (runAndRollback testOrphans)
        ]
    ]

type DummySeal = Text

defaultGenesis :: Block tx DummySeal
defaultGenesis =
    emptyGenesisBlock Time.epoch $> mempty

runAndRollback :: (Handle RadTx DummySeal -> IO a) -> IO a
runAndRollback action =
    bracket (open ":memory:" >>= initialize defaultGenesis)
            close
            action

testStoreLookupBlock :: Handle RadTx DummySeal -> Assertion
testStoreLookupBlock h = do
    blk <- generate arbitraryBlock

    storeBlock h blk
    Just blk' <- lookupBlock h (blockHash blk)

    blk' @?= blk

testStoreLookupTx :: Handle RadTx DummySeal -> Assertion
testStoreLookupTx h = do
    blk <- generate $ arbitraryBlock `suchThat` (not . null . blockData)

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
    blks <- replicateM 10 (generate arbitraryBlock)

    for_ blks (storeBlock h)

    blks' <- orphans h

    -- This expectedly fails because `orphans` currently returns the genesis
    -- block.
    length blks' @?= length blks

testMaximumChain :: Handle RadTx DummySeal -> Assertion
testMaximumChain h = do
    gen <- getGenesisBlock h

    blks :: Blockchain RadTx DummySeal <-
        generate $ resize 1 $ arbitraryValidBlockchainFrom gen

    for_ (NonEmpty.init $ fromBlockchain blks) (storeBlock h)

    blks' <- maximumChainBy h (\_ _ -> EQ)

    fromBlockchain blks @?= fromBlockchain blks'
