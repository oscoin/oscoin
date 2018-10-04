{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Blockchain.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Test.Crypto.Hash.Arbitrary ()

import           Oscoin.Clock
import           Oscoin.Consensus.Evaluator (Evaluator, identityEval)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash (HashAlgorithm, hash, hashAlgorithm)

import qualified Crypto.Hash as Crypto

import           Codec.Serialise (Serialise)
import           Control.Monad (replicateM)
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import           Data.Word (Word8)

import           Oscoin.Test.Clock ()
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

arbitraryValidBlock :: forall tx s. (Serialise tx, Arbitrary tx, Arbitrary s) => Blockchain tx s -> Gen (Block tx s)
arbitraryValidBlock (Blockchain (Block prevHeader _ :| _)) = do
    txs <- arbitrary :: Gen [tx]
    arbitraryValidBlockWith (void prevHeader) txs

arbitraryValidBlockWith :: (Serialise tx, Arbitrary s) => BlockHeader () -> [tx] -> Gen (Block tx s)
arbitraryValidBlockWith prevHeader txs = do
    elapsed    <- choose (2750 * seconds, 3250 * seconds)
    blockState <- arbitrary
    let header = emptyHeader
               { blockPrevHash   = hash prevHeader
               , blockDataHash   = hashTxs txs
               , blockState
               , blockTimestamp  = blockTimestamp prevHeader `timeAdd` elapsed
               , blockDifficulty = 0
               }
    pure $ Block header (Seq.fromList txs)

arbitraryGenesis :: forall tx s. (Serialise tx, Arbitrary tx, Arbitrary s) => Gen (Block tx s)
arbitraryGenesis = do
    txs <- resize 20 arbitrary :: Gen [tx]
    arbitraryGenesisWith identityEval txs

arbitraryGenesisWith
    :: forall tx s
     . (Serialise tx, Arbitrary s)
     => Evaluator s tx ()
     -> [tx]
     -> Gen (Block tx s)
arbitraryGenesisWith eval txs = do
    phil <- genesisBlock @[] @tx @s <$> arbitrary <*> pure eval <*> arbitrary <*> pure txs
    case phil of
        Right blk -> pure blk
        Left  err -> panic $ "Failed to generate genesis: " <> show err

arbitraryEmptyGenesis
    :: forall tx s. (Serialise tx, Arbitrary s) => Gen (Block tx s)
arbitraryEmptyGenesis =
    emptyGenesisBlock <$> arbitrary <*> arbitrary

arbitraryValidBlockchain :: (Serialise tx, Arbitrary tx, Arbitrary s) => s -> Gen (Blockchain tx s)
arbitraryValidBlockchain s = do
    gen <- emptyGenesisBlock <$> arbitrary <*> pure s
    h   <- choose (8, 9) :: Gen Int
    go (gen :| []) h
  where
    go blks 0 =
        pure $ Blockchain blks
    go blks n = do
        blk <- arbitraryValidBlock (Blockchain blks)
        go (blk <| blks) (n - 1)
