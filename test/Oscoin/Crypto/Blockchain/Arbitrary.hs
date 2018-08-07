{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Crypto.Blockchain.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Hash.Arbitrary ()
import           Oscoin.Consensus.Evaluator (Evaluator, identityEval)

import qualified Crypto.Hash as Crypto

import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import           Data.Binary (Binary)
import           Data.Word (Word8)
import           Control.Monad (replicateM)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

arbitraryBlockchain :: forall tx. (Arbitrary tx, Binary tx) => Gen (Blockchain tx ())
arbitraryBlockchain = do
    gen  <- Blockchain . singleton <$> arbitraryGenesis
    rest <- arbitraryValidBlock gen
    pure $ rest |> gen
  where
    singleton x = x :| []

instance Arbitrary (Crypto.Digest HashAlgorithm) where
    arbitrary = do
        str <- replicateM (Crypto.hashDigestSize hashAlgorithm) (arbitrary :: Gen Word8)
        pure . fromJust $ Crypto.digestFromByteString (BS.pack str)

arbitraryValidBlock :: forall tx s. (Binary tx, Arbitrary tx, Monoid s) => Blockchain tx s -> Gen (Block tx s)
arbitraryValidBlock (Blockchain (Block prevHeader _ :| _)) = do
    txs <- arbitrary :: Gen [tx]
    arbitraryValidBlockWith (void prevHeader) txs

arbitraryValidBlockWith :: (Binary tx, Monoid s) => BlockHeader () -> [tx] -> Gen (Block tx s)
arbitraryValidBlockWith prevHeader txs = do
    elapsed <- choose (2750, 3250)
    let header = emptyHeader
               { blockPrevHash   = hash prevHeader
               , blockDataHash   = hashTxs txs
               , blockState      = mempty
               , blockStateHash  = zeroHash
               , blockTimestamp  = blockTimestamp prevHeader + elapsed
               , blockDifficulty = 0
               }
    pure $ Block header (Seq.fromList txs)

arbitraryGenesis :: forall tx s. (Binary tx, Arbitrary tx, Monoid s) => Gen (Block tx s)
arbitraryGenesis = do
    txs <- resize 20 arbitrary :: Gen [tx]
    arbitraryGenesisWith identityEval txs

arbitraryGenesisWith
    :: forall tx s
     . (Binary tx, Monoid s)
     => Evaluator s tx ()
     -> [tx]
     -> Gen (Block tx s)
arbitraryGenesisWith eval txs =
    map fromJust $ genesisBlock @[] @tx @s mempty eval <$> arbitrary <*> pure txs

arbitraryEmptyGenesis
    :: forall tx s. (Binary tx, Monoid s) => Gen (Block tx s)
arbitraryEmptyGenesis =
    emptyGenesisBlock <$> arbitrary

arbitraryValidBlockchain :: (Binary tx, Arbitrary tx, Monoid s) => Gen (Blockchain tx s)
arbitraryValidBlockchain = do
    gen <- arbitraryEmptyGenesis
    h   <- choose (8, 9) :: Gen Int
    go (gen :| []) h
  where
    go blks 0 =
        pure $ Blockchain blks
    go blks n = do
        blk <- arbitraryValidBlock (Blockchain blks)
        go (blk <| blks) (n - 1)
