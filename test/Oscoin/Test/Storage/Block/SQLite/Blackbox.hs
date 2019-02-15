{- | Blackbox testing for the SQLite Blockstore via its abstract interface. -}
module Oscoin.Test.Storage.Block.SQLite.Blackbox
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
                 (Blockchain(..), blocks, chainLength, txPayload)
import           Oscoin.Crypto.Blockchain.Block hiding (genesisBlock)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.RadicleTx
import qualified Oscoin.Storage.Block.Abstract as Abstract

import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary
                 (arbitraryBlock, arbitraryBlockWith)
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Blockchain.Generators (genBlockchainFrom)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.Storage.Block.SQLite

import qualified Data.Set as Set

import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testGroup "Storage.Block"
        [ testProperty "Store/lookup Block" (withMemStore genBlockFrom testStoreLookupBlock)
        , testProperty "Store/lookup Tx"    (withMemStore genNonEmptyBlock testStoreLookupTx)
        , testProperty "Get Genesis Block"  (withMemStore (const arbitrary) testGetGenesisBlock)
        , testProperty "Get blocks"         (withMemStore genGetBlocks testGetBlocks)
        , testProperty "Orphans"            (withMemStore (const (vectorOf 10 (arbitraryBlockWith []))) testOrphans)
        ]
    ]

{------------------------------------------------------------------------------
  Generators tailored for the tests at hand
------------------------------------------------------------------------------}

-- | Generates a non-empty 'Block'.
genNonEmptyBlock :: Block RadTx DummySeal -> Gen (Block RadTx DummySeal)
genNonEmptyBlock genesisBlock =
    genBlockFrom genesisBlock `suchThat` (not . null . blockData)


genGetBlocks :: Block RadTx DummySeal
             -> Gen (Block RadTx DummySeal, Blockchain RadTx DummySeal)
genGetBlocks genesisBlock =
    (,) <$> arbitraryBlock
        <*> resize 1 (genBlockchainFrom genesisBlock)


{------------------------------------------------------------------------------
  The tests proper
------------------------------------------------------------------------------}

testStoreLookupBlock :: Block RadTx DummySeal
                     -> Abstract.BlockStore RadTx DummySeal IO
                     -> Assertion
testStoreLookupBlock blk h = do
    Abstract.insertBlock h blk
    Just blk' <- Abstract.lookupBlock h (blockHash blk)

    blk' @?= blk

testStoreLookupTx :: Block RadTx DummySeal
                  -> Abstract.BlockStore RadTx DummySeal IO
                  -> Assertion
testStoreLookupTx blk h = do
    Abstract.insertBlock h blk

    let tx = headDef (panic "No transactions!")
           $ toList $ blockData blk

    Just tx' <- Abstract.lookupTx h (Crypto.hash tx)

    txPayload tx' @?= tx

testGetGenesisBlock :: () -> Abstract.BlockStore RadTx DummySeal IO -> Assertion
testGetGenesisBlock () h = do
    blk <- Abstract.getGenesisBlock h
    defaultGenesis @?= blk

testOrphans :: [Block RadTx DummySeal]
            -> Abstract.BlockStore RadTx DummySeal IO
            -> Assertion
testOrphans blks h = do
    for_ blks (Abstract.insertBlock h)
    blks' <- Abstract.getOrphans h

    blks' @?= Set.fromList (map blockHash blks)

testGetBlocks :: (Block RadTx DummySeal, Blockchain RadTx DummySeal)
              -> Abstract.BlockStore RadTx DummySeal IO
              -> Assertion
testGetBlocks (block, chain) h = do
    Abstract.insertBlocksNaive h (initDef [] $ blocks chain)

    -- This orphan block shouldn't be returned by 'maximumChainBy'.
    Abstract.insertBlock h block

    blks' <- Abstract.getBlocks h (fromIntegral $ chainLength chain)

    blocks chain @?= blks'
