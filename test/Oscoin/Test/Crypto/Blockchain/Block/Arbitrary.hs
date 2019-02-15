{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Crypto.Blockchain.Block.Arbitrary
    ( arbitraryBlock
    , arbitraryBlockWith
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash (Hash)

import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()
import           Test.QuickCheck

instance Arbitrary Difficulty where
    arbitrary = arbitraryDifficulty

instance (Serialise tx, Serialise s, Arbitrary tx, Arbitrary s) => Arbitrary (Block tx s) where
    arbitrary = arbitraryBlock

instance Arbitrary Nakamoto.PoW where
    arbitrary = Nakamoto.PoW <$> arbitrary

-- | Generates an arbitrary block where the parent hash is randomly generated.
-- Callers of this function needs to be aware that generating a block this way
-- doesn't give them any guarantee in terms of creating a well-constructed
-- blockchain. Use this function only when you know what you are doing. To
-- generate well-formed blocks and blockchains, take a look at 'genBlockFrom' and
-- 'genBlockchainFrom'.
arbitraryBlock
    :: forall tx s. (Serialise s, Serialise tx, Arbitrary tx, Arbitrary s)
    => Gen (Block tx s)
arbitraryBlock =
    arbitraryBlockWith =<< arbitrary


-- | Generates a random 'Difficulty' without considering the previous one.
arbitraryDifficulty :: Gen Difficulty
arbitraryDifficulty =
    (unsafeDifficulty . getPositive <$> arbitrary) `suchThat` noOverflow
  where noOverflow d = d <= maxDifficulty


-- | Generates a 'Block' from a given list of transactions. The same caveats
-- as 'arbitraryBlock' apply.
arbitraryBlockWith
    :: forall tx s. (Serialise s, Serialise tx, Arbitrary s)
    => [tx]
    -> Gen (Block tx s)
arbitraryBlockWith txs = do
    timestamp <- arbitrary
    diffi <- arbitraryDifficulty
    prevHash <- arbitrary :: Gen Hash
    stateHash <- arbitrary :: Gen Hash
    blockSeal  <- arbitrary

    let header = emptyHeader
               { blockPrevHash   = prevHash
               , blockDataHash   = hashTxs txs
               , blockStateHash  = stateHash
               , blockSeal
               , blockTimestamp  = timestamp
               , blockTargetDifficulty = diffi
               }
    pure $ mkBlock header txs
