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

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary
                 (arbitraryBlock, arbitraryBlockWith)
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Blockchain.Generators (genBlockchainFrom)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.Storage.Block.SQLite

import           Data.ByteArray.Orphans ()
import qualified Data.Set as Set

import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> [TestTree]
tests Dict =
    [ testGroup "Storage.Block"
        [ testProperty "Store/lookup Block" (withMemStore genBlockFrom (testStoreLookupBlock @c))
        , testProperty "Store/lookup Tx"    (withMemStore genNonEmptyBlock (testStoreLookupTx @c))
        , testProperty "Get Genesis Block"  (withMemStore (const arbitrary) (testGetGenesisBlock @c))
        , testProperty "Get blocks"         (withMemStore genGetBlocks (testGetBlocks @c))
        , testProperty "Orphans"            (withMemStore (const (vectorOf 10 (arbitraryBlockWith []))) (testOrphans @c))
        ]
    ]

{------------------------------------------------------------------------------
  Generators tailored for the tests at hand
------------------------------------------------------------------------------}

-- | Generates a non-empty 'Block'.
genNonEmptyBlock
    :: IsCrypto c
    => Block c (RadTx c) (Sealed c DummySeal)
    -> Gen (Block c (RadTx c) (Sealed c DummySeal))
genNonEmptyBlock genesisBlock =
    genBlockFrom genesisBlock `suchThat` (not . null . blockData)


genGetBlocks
    :: IsCrypto c
    => Block c (RadTx c) (Sealed c DummySeal)
    -> Gen (Block c (RadTx c) (Sealed c DummySeal), Blockchain c (RadTx c) DummySeal)
genGetBlocks genesisBlock =
    (,) <$> arbitraryBlock
        <*> resize 1 (genBlockchainFrom genesisBlock)


{------------------------------------------------------------------------------
  The tests proper
------------------------------------------------------------------------------}

testStoreLookupBlock
    :: IsCrypto c
    => Block c (RadTx c) (Sealed c DummySeal)
    -> Abstract.BlockStore c (RadTx c) DummySeal IO
    -> Assertion
testStoreLookupBlock blk h = do
    Abstract.insertBlock h blk
    Just blk' <- Abstract.lookupBlock h (blockHash blk)

    blk' @?= blk

testStoreLookupTx
    :: IsCrypto c
    => Block c (RadTx c) (Sealed c DummySeal)
    -> Abstract.BlockStore c (RadTx c) DummySeal IO
    -> Assertion
testStoreLookupTx blk h = do
    Abstract.insertBlock h blk

    let tx = headDef (panic "No transactions!")
           $ toList $ blockData blk

    Just tx' <- Abstract.lookupTx h (Crypto.hash tx)

    txPayload tx' @?= tx

testGetGenesisBlock
    :: IsCrypto c
    => ()
    -> Abstract.BlockStore c (RadTx c) DummySeal IO -> Assertion
testGetGenesisBlock () h = do
    blk <- Abstract.getGenesisBlock h
    defaultGenesis @?= blk

testOrphans
    :: IsCrypto c
    => [Block c (RadTx c) (Sealed c DummySeal)]
    -> Abstract.BlockStore c (RadTx c) DummySeal IO
    -> Assertion
testOrphans blks h = do
    for_ blks (Abstract.insertBlock h)
    blks' <- Abstract.getOrphans h

    blks' @?= Set.fromList (map blockHash blks)

testGetBlocks
    :: IsCrypto c
    => (Block c (RadTx c) (Sealed c DummySeal), Blockchain c (RadTx c) DummySeal)
    -> Abstract.BlockStore c (RadTx c) DummySeal IO
    -> Assertion
testGetBlocks (block, chain) h = do
    Abstract.insertBlocksNaive h (initDef [] $ blocks chain)

    -- This orphan block shouldn't be returned by 'maximumChainBy'.
    Abstract.insertBlock h block

    blks' <- Abstract.getBlocks h (fromIntegral $ chainLength chain)

    blocks chain @?= blks'
