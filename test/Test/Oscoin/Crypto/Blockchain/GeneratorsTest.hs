module Test.Oscoin.Crypto.Blockchain.GeneratorsTest
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Validation
import           Oscoin.Crypto.Blockchain
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Generators
import qualified Oscoin.Time.Chrono as Chrono

import           Hedgehog
import           Hedgehog.Gen.QuickCheck
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: Dict (IsCrypto c) -> TestTree
tests crypto = testGroup "Test.Oscoin.Crypto.Blockchain.GeneratorsTest"
    [ testProperty "prop_genNakamotoBlockchainValid" $ prop_genNakamotoBlockchainValid crypto
    , testProperty "prop_genBlockchainLinked" $ prop_genBlockchainLinked crypto
    ]

-- | Tests that the blockchains generated with 'genNakamotoBlockchain'
-- validate with 'Nakamoto.validateFull'. This does not check the
-- proof-of-work.
prop_genNakamotoBlockchainValid :: forall c. Dict (IsCrypto c) -> Property
prop_genNakamotoBlockchainValid Dict = property $ do
    (blockchain :: Blockchain c () Nakamoto.PoW) <-
        forAllWith (toS . showChainDigest) (quickcheck genNakamotoBlockchain)
    validateBlockchain Nakamoto.validateFull blockchain === Right ()

-- | Tests that the blockchains generated with 'genBlockchain' are
-- properly linked. That is that the parent hash contained in the block
-- header is the block hash of the previous block.
prop_genBlockchainLinked :: forall c. Dict (IsCrypto c) -> Property
prop_genBlockchainLinked Dict = property $ do
    blockchain :: Blockchain c () () <-
        forAllWith (toS . showChainDigest) (quickcheck genBlockchain)
    blocksLinked (Chrono.toNewestFirst $ blocks blockchain)
  where
    blocksLinked :: (MonadTest m) => [Block c tx s] -> m ()
    blocksLinked []             = pure ()
    blocksLinked [_]            = pure ()
    blocksLinked (blk: suffix@(parent:_)) = do
        runExcept (validateParentHash parent blk) === Right ()
        blocksLinked suffix
