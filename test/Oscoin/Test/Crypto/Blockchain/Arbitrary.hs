{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Blockchain.Arbitrary
    ( arbitraryBlockchain
    , arbitraryValidBlockchain
    , arbitraryValidBlockchainFrom
    , arbitraryBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock)
import           Oscoin.Crypto.Hash (Hash, hash, toHashed)
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.Sequence as Seq

import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

arbitraryBlockchain :: (Arbitrary tx, Serialise tx, Arbitrary s) => Gen (Blockchain tx s)
arbitraryBlockchain = do
    chain <- fromGenesis . emptyGenesisBlock epoch <$> arbitrary
    rest <- arbitraryValidBlock chain
    pure $ rest |> chain

arbitraryBlock :: forall tx. (Serialise tx, Arbitrary tx) => Gen (Block tx ())
arbitraryBlock = do
    txs <- arbitrary :: Gen [tx]
    timestamp <- arbitrary
    diffi <- arbitrary
    prevHash <- arbitrary :: Gen Hash

    let header = emptyHeader
               { blockPrevHash   = toHashed prevHash
               , blockDataHash   = hashTxs txs
               , blockState      = ()
               , blockTimestamp  = timestamp
               , blockDifficulty = diffi
               }
    pure $ mkBlock header txs

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

arbitraryValidBlockchain :: (Serialise tx, Arbitrary tx, Arbitrary s) => s -> Gen (Blockchain tx s)
arbitraryValidBlockchain s = do
    ts <- arbitrary
    arbitraryValidBlockchainFrom (emptyGenesisBlock ts s)

arbitraryValidBlockchainFrom :: (Serialise tx, Arbitrary tx, Arbitrary s) => Block tx s -> Gen (Blockchain tx s)
arbitraryValidBlockchainFrom gen = do
    h <- choose (8, 9) :: Gen Int
    go (gen :| []) h
  where
    go blks 0 =
        pure $ Blockchain blks
    go blks n = do
        blk <- arbitraryValidBlock (Blockchain blks)
        go (blk <| blks) (n - 1)
