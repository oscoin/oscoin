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
import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.Sequence as Seq

import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

arbitraryBlockchain :: (Arbitrary tx, Serialise tx) => Gen (Blockchain tx ())
arbitraryBlockchain = do
    chain <- pure $ fromGenesis $ emptyGenesisBlock epoch
    rest <- arbitraryValidBlock chain
    pure $ rest |> chain

arbitraryBlock :: forall tx s. (Serialise tx, Arbitrary tx, Arbitrary s) => Gen (Block tx s)
arbitraryBlock = do
    txs <- arbitrary :: Gen [tx]
    timestamp <- arbitrary
    diffi <- arbitrary
    prevHash <- arbitrary :: Gen Hash
    stateHash <- arbitrary :: Gen Hash
    blockSeal  <- arbitrary

    let header = emptyHeader
               { blockPrevHash   = prevHash
               , blockDataHash   = hashTxs txs
               , blockStateHash  = stateHash
               , blockSeal
               , blockTimestamp  = timestamp
               , blockDifficulty = diffi
               }
    pure $ mkBlock header txs

arbitraryValidBlock :: forall tx s. (Serialise tx, Serialise s, Arbitrary tx, Arbitrary s) => Blockchain tx s -> Gen (Block tx s)
arbitraryValidBlock (Blockchain (Block prevHeader _ :| _)) = do
    txs <- arbitrary :: Gen [tx]
    arbitraryValidBlockWith prevHeader txs

arbitraryValidBlockWith :: (Serialise tx, Serialise s, Arbitrary s) => BlockHeader s -> [tx] -> Gen (Block tx s)
arbitraryValidBlockWith prevHeader txs = do
    elapsed    <- choose (2750 * seconds, 3250 * seconds)
    blockState <- arbitrary :: Gen Word8
    blockSeal  <- arbitrary
    let header = emptyHeader
               { blockPrevHash   = headerHash prevHeader
               , blockDataHash   = hashTxs txs
               , blockStateHash  = hashState blockState
               , blockSeal
               , blockTimestamp  = blockTimestamp prevHeader `timeAdd` elapsed
               , blockDifficulty = 0
               }
    pure $ Block header (Seq.fromList txs)

arbitraryValidBlockchain :: (Serialise tx, Serialise s, Arbitrary tx, Arbitrary s) => Gen (Blockchain tx s)
arbitraryValidBlockchain = do
    ts <- arbitrary
    seal <- arbitrary
    arbitraryValidBlockchainFrom (emptyGenesisBlock ts $> seal)

arbitraryValidBlockchainFrom :: (Serialise tx, Serialise s, Arbitrary tx, Arbitrary s) => Block tx s -> Gen (Blockchain tx s)
arbitraryValidBlockchainFrom gen = do
    h <- choose (8, 9) :: Gen Int
    go (gen :| []) h
  where
    go blks 0 =
        pure $ Blockchain blks
    go blks n = do
        blk <- arbitraryValidBlock (Blockchain blks)
        go (blk <| blks) (n - 1)
