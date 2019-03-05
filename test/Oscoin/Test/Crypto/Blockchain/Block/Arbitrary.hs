{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Test.Crypto.Blockchain.Block.Arbitrary
    ( arbitraryBlock
    , arbitraryBlockWith
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash (Hash)

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()
import           Test.QuickCheck

instance Arbitrary s => Arbitrary (Sealed c s) where
    arbitrary = SealedWith <$> arbitrary

instance Arbitrary Difficulty where
    arbitrary = arbitraryDifficulty

instance ( Serialise tx
         , Serialise s
         , Arbitrary tx
         , Arbitrary s
         , IsCrypto c
         ) => Arbitrary (Block c tx s) where
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
    :: (IsCrypto c, Serialise s, Serialise tx, Arbitrary tx, Arbitrary s)
    => Gen (Block c tx s)
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
    :: forall c tx s. (IsCrypto c, Serialise s, Serialise tx, Arbitrary s)
    => [tx]
    -> Gen (Block c tx s)
arbitraryBlockWith txs = do
    timestamp <- arbitrary
    diffi <- arbitraryDifficulty
    prevHash <- arbitrary :: Gen (Hash c)
    stateHash <- arbitrary :: Gen (Hash c)
    blockSeal <- arbitrary

    let header = (emptyHeader :: BlockHeader c Unsealed)
               { blockPrevHash   = prevHash
               , blockDataHash   = hashTxs txs
               , blockStateHash  = stateHash
               , blockTimestamp  = timestamp
               , blockSeal
               , blockTargetDifficulty = diffi
               }
    pure $ mkBlock header txs
