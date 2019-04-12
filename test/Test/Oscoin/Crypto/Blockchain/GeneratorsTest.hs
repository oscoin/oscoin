module Test.Oscoin.Crypto.Blockchain.GeneratorsTest
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Validation
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Time.Chrono as Chrono

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Generators
import           Test.Oscoin.DummyLedger

import           Hedgehog
import qualified Hedgehog.Gen.QuickCheck as Gen
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: Dict (IsCrypto c) -> TestTree
tests crypto = testGroup "Test.Oscoin.Crypto.Blockchain.GeneratorsTest"
    [ testProperty "prop_genNakamotoBlockchainValid" $ prop_genNakamotoBlockchainValid crypto
    , testProperty "prop_genBlockchainLinked" $ prop_genBlockchainLinked crypto
    , testProperty "prop_genEvaledBlockchainStateHash" $ prop_genEvaledBlockchainStateHash crypto
    ]

-- | Tests that the blockchains generated with 'genNakamotoBlockchain'
-- validate with 'Nakamoto.validateFull'. This does not check the
-- proof-of-work.
prop_genNakamotoBlockchainValid :: forall c. Dict (IsCrypto c) -> Property
prop_genNakamotoBlockchainValid Dict = property $ do
    (blockchain :: Blockchain c () Nakamoto.PoW) <-
        forAllWith (toS . showChainDigest) (Gen.quickcheck genNakamotoBlockchain)
    validateBlockchain Nakamoto.validateFull blockchain === Right ()

-- | Tests that the blockchains generated with 'genBlockchain' are
-- properly linked. That is that the parent hash contained in the block
-- header is the block hash of the previous block.
prop_genBlockchainLinked :: forall c. Dict (IsCrypto c) -> Property
prop_genBlockchainLinked Dict = property $ do
    blockchain :: Blockchain c () () <-
        forAllWith (toS . showChainDigest) (Gen.quickcheck genBlockchain)
    blocksLinked (Chrono.toNewestFirst $ blocks blockchain)
  where
    blocksLinked :: (MonadTest m) => [Block c tx s] -> m ()
    blocksLinked []             = pure ()
    blocksLinked [_]            = pure ()
    blocksLinked (blk: suffix@(parent:_)) = do
        runExcept (validateParentHash parent blk) === Right ()
        blocksLinked suffix


-- | Tests that the blocks in a blockchain generated with
-- 'genEvaledBlockchain' have the correct state hash. That is the state
-- hash of a block is computed from the state that results from
-- applying the block’s transactions to the previous state.
prop_genEvaledBlockchainStateHash :: forall c. Dict (IsCrypto c) -> Property
prop_genEvaledBlockchainStateHash Dict = property $ do
    let initialState = []
    blockchain :: Blockchain c DummyTx () <-
        forAll $ Gen.quickcheck $ genEvaledBlockchain dummyEval initialState
    let blks = Chrono.toOldestFirst $ Chrono.reverse $ blocks blockchain
    foldM_ validateStateHash initialState blks
  where
    validateStateHash :: (MonadTest m) => DummyState -> Block c DummyTx s -> m DummyState
    validateStateHash prevState blk = do
        let (blkState, _) = evalBlock dummyEval prevState blk
        Crypto.fromHashed (Crypto.hash blkState) === blockStateHash (blockHeader blk)
        pure blkState
