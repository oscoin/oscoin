{- | Blackbox testing for the SQLite Blockstore via its abstract interface. -}
module Oscoin.Test.Storage.Block.SQLite.Blackbox
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
                 (Blockchain(..), blocks, chainLength, height, tip, txPayload)
import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.Tx
import qualified Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Time.Chrono as Chrono

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary ()
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Blockchain.Generators (genBlockchainFrom)
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.Storage.Block.SQLite

import           Data.ByteArray.Orphans ()
import           Data.List ((!!))

import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> [TestTree]
tests Dict =
    [ testGroup "Storage.Block"
        [ testProperty "Store/lookup Block"   (withMemStore genBlockFrom (testStoreLookupBlock @c))
        , testProperty "lookupBlockByHeight"  (withMemStore genBlockchainFrom (testLookupBlockByHeight @c))
        , testProperty "lookupBlocksByHeight" (withMemStore genBlockchainFrom (testLookupBlocksByHeight @c))
        , testProperty "Store/lookup Tx"      (withMemStore genNonEmptyBlock (testStoreLookupTx @c))
        , testProperty "Get Genesis Block"    (withMemStore pure (testGetGenesisBlock @c))
        , testProperty "Get blocks"           (withMemStore genGetBlocks (testGetBlocks @c))
        ]
    ]

{------------------------------------------------------------------------------
  Generators tailored for the tests at hand
------------------------------------------------------------------------------}

-- | Generates a non-empty 'Block'.
genNonEmptyBlock
    :: IsCrypto c
    => Block c (Tx c) (Sealed c DummySeal)
    -> Gen (Block c (Tx c) (Sealed c DummySeal))
genNonEmptyBlock genesisBlock =
    genBlockFrom genesisBlock `suchThat` (not . null . blockTxs)


genGetBlocks
    :: IsCrypto c
    => Block c (Tx c) (Sealed c DummySeal)
    -> Gen (Block c (Tx c) (Sealed c DummySeal), Blockchain c (Tx c) DummySeal)
genGetBlocks genesisBlock =
    (,) <$> genStandaloneBlock
        <*> resize 1 (genBlockchainFrom genesisBlock)


{------------------------------------------------------------------------------
  The tests proper
------------------------------------------------------------------------------}

testStoreLookupBlock
    :: IsCrypto c
    => Block c (Tx c) (Sealed c DummySeal)
    -> Abstract.BlockStore c (Tx c) DummySeal IO
    -> Assertion
testStoreLookupBlock blk (publicAPI, privateAPI) = do
    Abstract.insertBlock privateAPI blk
    Just blk' <- Abstract.lookupBlock publicAPI (blockHash blk)

    blk' @?= blk

testLookupBlockByHeight
    :: IsCrypto c
    => Blockchain c (Tx c) DummySeal
    -> Abstract.BlockStore c (Tx c) DummySeal IO
    -> Assertion
testLookupBlockByHeight chain (publicAPI, privateAPI) = do
    let allBlocks    = Chrono.reverse $ blocks chain
    let targetHeight = pred . blockHeight . blockHeader . tip $ chain
    Abstract.insertBlocksNaive privateAPI allBlocks
    blk' <- Abstract.lookupBlockByHeight publicAPI targetHeight

    blk' @?= Just (Chrono.toOldestFirst allBlocks !! (fromIntegral targetHeight))

testLookupBlocksByHeight
    :: IsCrypto c
    => Blockchain c (Tx c) DummySeal
    -> Abstract.BlockStore c (Tx c) DummySeal IO
    -> Assertion
testLookupBlocksByHeight chain (publicAPI, privateAPI) = do
    let allBlocks  = Chrono.reverse $ blocks chain
    let rangeEnd   = pred . height $ chain
    let tipless    = Chrono.OldestFirst
                   . reverse
                   . drop 1
                   . Chrono.toNewestFirst
                   . blocks $ chain
    Abstract.insertBlocksNaive privateAPI allBlocks

    -- We test the happy path scenario first, i.e. that all the requested
    -- blocks are there.
    blks <- Abstract.lookupBlocksByHeight publicAPI (0, rangeEnd)
    blks @?= tipless

    -- We now test that if the /end/ of the range is beyond the max height in
    -- the chain, the system returns only the available blocks (basically, the
    -- whole chain)
    fullRange <- Abstract.lookupBlocksByHeight publicAPI (0, rangeEnd + 100)
    fullRange @?= Chrono.reverse (blocks chain)

    -- Finally we test that if we request an invalid range (i.e. start > end)
    -- nothing is returned.
    none <- Abstract.lookupBlocksByHeight publicAPI (rangeEnd + 100, 0)
    none @?= mempty

testStoreLookupTx
    :: IsCrypto c
    => Block c (Tx c) (Sealed c DummySeal)
    -> Abstract.BlockStore c (Tx c) DummySeal IO
    -> Assertion
testStoreLookupTx blk (publicAPI, privateAPI) = do
    Abstract.insertBlock privateAPI blk

    let tx = headDef (panic "No transactions!")
           $ toList $ blockTxs blk

    Just tx' <- Abstract.lookupTx publicAPI (Crypto.hash tx)

    txPayload tx' @?= tx

testGetGenesisBlock
    :: IsCrypto c
    => Block c (Tx c) (Sealed c DummySeal)
    -> Abstract.BlockStore c (Tx c) DummySeal IO -> Assertion
testGetGenesisBlock genesis (publicAPI, _privateAPI) = do
    blk <- Abstract.getGenesisBlock publicAPI
    genesis @?= blk

testGetBlocks
    :: IsCrypto c
    => (Block c (Tx c) (Sealed c DummySeal), Blockchain c (Tx c) DummySeal)
    -> Abstract.BlockStore c (Tx c) DummySeal IO
    -> Assertion
testGetBlocks (block, chain) (publicAPI, privateAPI) = do
    Abstract.insertBlocksNaive privateAPI (Chrono.reverse $ blocks chain)

    -- This orphan block shouldn't be returned by 'maximumChainBy'.
    Abstract.insertBlock privateAPI block

    blks' <- Abstract.getBlocksByDepth publicAPI (fromIntegral $ chainLength chain)

    blocks chain @?= blks'
