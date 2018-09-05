{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Blockchain.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Test.Crypto.Hash.Arbitrary ()

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash
import           Oscoin.Consensus.Evaluator (Evaluator, identityEval)

import qualified Crypto.Hash as Crypto

import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import           Codec.Serialise (Serialise)
import           Data.Word (Word8)
import           Control.Monad (replicateM)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

arbitraryBlockchain :: forall tx. (Arbitrary tx, Serialise tx) => Gen (Blockchain tx ())
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

arbitraryValidBlock :: forall tx s. (Serialise tx, Arbitrary tx, Default s) => Blockchain tx s -> Gen (Block tx s)
arbitraryValidBlock (Blockchain (Block prevHeader _ :| _)) = do
    txs <- arbitrary :: Gen [tx]
    arbitraryValidBlockWith (void prevHeader) txs

arbitraryValidBlockWith :: (Serialise tx, Default s) => BlockHeader () -> [tx] -> Gen (Block tx s)
arbitraryValidBlockWith prevHeader txs = do
    elapsed <- choose (2750, 3250)
    let header = emptyHeader
               { blockPrevHash   = hash prevHeader
               , blockDataHash   = hashTxs txs
               , blockState      = def
               , blockStateHash  = zeroHash
               , blockTimestamp  = blockTimestamp prevHeader + elapsed
               , blockDifficulty = 0
               }
    pure $ Block header (Seq.fromList txs)

arbitraryGenesis :: forall tx s. (Serialise tx, Arbitrary tx, Default s) => Gen (Block tx s)
arbitraryGenesis = do
    txs <- resize 20 arbitrary :: Gen [tx]
    arbitraryGenesisWith identityEval txs

arbitraryGenesisWith
    :: forall tx s
     . (Serialise tx, Default s)
     => Evaluator s tx ()
     -> [tx]
     -> Gen (Block tx s)
arbitraryGenesisWith eval txs =
    map fromJust $ genesisBlock @[] @tx @s def eval <$> arbitrary <*> pure txs

arbitraryEmptyGenesis
    :: forall tx s. (Serialise tx, Default s) => Gen (Block tx s)
arbitraryEmptyGenesis =
    emptyGenesisBlock <$> arbitrary

arbitraryValidBlockchain :: (Serialise tx, Arbitrary tx, Default s) => Gen (Blockchain tx s)
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
