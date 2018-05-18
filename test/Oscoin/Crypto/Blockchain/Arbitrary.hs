module Oscoin.Crypto.Blockchain.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.Hash.Arbitrary ()

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

instance (Arbitrary tx, Binary tx) => Arbitrary (Block tx) where
    arbitrary =
        Block <$> arbitrary <*> arbitrary

arbitraryBlockchain :: forall tx. (Arbitrary tx, Binary tx) => Gen (Blockchain tx)
arbitraryBlockchain = do
    genesis <- arbitraryGenesis
    rest <- arbitrary :: Gen [Block tx]
    pure $ Blockchain $ genesis :| rest

instance Arbitrary (Crypto.Digest HashAlgorithm) where
    arbitrary = do
        str <- replicateM (Crypto.hashDigestSize hashAlgorithm) (arbitrary :: Gen Word8)
        pure . fromJust $ Crypto.digestFromByteString (BS.pack str)

instance Arbitrary BlockHeader where
    arbitrary =
        BlockHeader
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

arbitraryValidBlock :: forall tx. (Binary tx, Arbitrary tx) => Blockchain tx -> Gen (Block tx)
arbitraryValidBlock (Blockchain (Block prevHeader _ :| _)) = do
    txs <- arbitrary :: Gen [tx]
    arbitraryValidBlockWith prevHeader txs

arbitraryValidBlockWith :: forall tx. Binary tx => BlockHeader -> [tx] -> Gen (Block tx)
arbitraryValidBlockWith prevHeader txs = do
    elapsed <- choose (2750, 3250)
    let header = emptyHeader
               { blockPrevHash   = hash prevHeader
               , blockRootHash   = hashTxs txs
               , blockTimestamp  = blockTimestamp prevHeader + elapsed
               , blockDifficulty = 0
               }
    pure $ Block header (Seq.fromList txs)

arbitraryGenesis :: forall tx. (Binary tx, Arbitrary tx) => Gen (Block tx)
arbitraryGenesis = do
    txs <- resize 20 arbitrary :: Gen [tx]
    arbitraryGenesisWith txs

arbitraryGenesisWith :: Binary tx => [tx] -> Gen (Block tx)
arbitraryGenesisWith txs = do
    g <- genesisBlock <$> arbitrary <*> pure txs
    pure $ g { blockHeader = blockHeader g }

arbitraryValidBlockchain :: forall tx. (Binary tx, Arbitrary tx) => Gen (Blockchain tx)
arbitraryValidBlockchain = do
    gen <- arbitraryGenesis :: (Gen (Block tx))
    h   <- choose (8, 9) :: Gen Int
    go (gen :| []) h
  where
    go blks 0 =
        pure $ Blockchain blks
    go blks n = do
        blk <- arbitraryValidBlock (Blockchain blks)
        go (blk <| blks) (n - 1)
