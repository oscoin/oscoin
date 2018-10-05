{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Blockchain.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock)
import           Oscoin.Crypto.Hash (HashAlgorithm, hash, hashAlgorithm)
import           Oscoin.Time

import qualified Crypto.Hash as Crypto

import           Codec.Serialise (Serialise)
import           Control.Monad (replicateM)
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import           Data.Word (Word8)

import           Oscoin.Test.Time ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

arbitraryBlockchain :: (Arbitrary tx, Serialise tx, Arbitrary s) => Gen (Blockchain tx s)
arbitraryBlockchain = do
    chain <- fromGenesis . emptyGenesisBlock epoch <$> arbitrary
    rest <- arbitraryValidBlock chain
    pure $ rest |> chain

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

arbitraryEmptyGenesis
    :: forall tx s. (Arbitrary s) => Gen (Block tx s)
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
