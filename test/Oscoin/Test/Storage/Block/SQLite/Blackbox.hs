{- | Blackbox testing for the SQLite Blockstore via its abstract interface. -}
module Oscoin.Test.Storage.Block.SQLite.Blackbox
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
                 (Blockchain(..), blocks, chainLength, txPayload)
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
    genBlockFrom genesisBlock `suchThat` (not . null . blockData)


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

testStoreLookupTx
    :: IsCrypto c
    => Block c (Tx c) (Sealed c DummySeal)
    -> Abstract.BlockStore c (Tx c) DummySeal IO
    -> Assertion
testStoreLookupTx blk (publicAPI, privateAPI) = do
    Abstract.insertBlock privateAPI blk

    let tx = headDef (panic "No transactions!")
           $ toList $ blockData blk

    Just tx' <- Abstract.lookupTx publicAPI (Crypto.hash tx)

    txPayload tx' @?= tx

testGetGenesisBlock
    :: IsCrypto c
    => ()
    -> Abstract.BlockStore c (Tx c) DummySeal IO -> Assertion
testGetGenesisBlock () (publicAPI, _privateAPI) = do
    blk <- Abstract.getGenesisBlock publicAPI
    defaultGenesis @?= blk

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
