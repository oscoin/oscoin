{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Blockchain.Arbitrary
    ( arbitraryBlockchain
    , arbitraryNakamotoBlockchain
    , arbitraryValidBlockchain
    , arbitraryValidBlockchainFrom
    , arbitraryValidBlockWith
    , arbitraryBlock
    , arbitraryBlockWith
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Mining (mineGenesis)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock)
import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Data.RadicleTx as Rad
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))

import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary Difficulty where
    arbitrary = Difficulty . getPositive <$> arbitrary

instance (Serialise tx, Serialise s, Arbitrary tx, Arbitrary s) => Arbitrary (Block tx s) where
    arbitrary = arbitraryBlock

instance Arbitrary Nakamoto.PoW where
    arbitrary = Nakamoto.PoW <$> arbitrary

arbitraryNakamotoGenesis :: (Serialise tx, Arbitrary tx) => Gen (Block tx Nakamoto.PoW)
arbitraryNakamotoGenesis = do
    txs <- arbitrary
    time <- arbitrary
    case buildGenesis identityEval time txs Rad.pureEnv of
        Left _    -> panic "Failed to generate arbitrary genesis block"
        Right blk -> do
            result <- mineGenesis
                (Nakamoto.mineNakamoto (const Nakamoto.minDifficulty)) blk
            case result of
                Left _     -> panic "Failed to generate arbirary genesis block"
                Right sblk -> pure sblk

arbitraryNakamotoBlockchain :: (Arbitrary tx, Serialise tx) => Gen (Blockchain tx Nakamoto.PoW)
arbitraryNakamotoBlockchain = do
    gen <- arbitraryNakamotoGenesis
    h <- choose (0, 10) :: Gen Int
    go (gen :| []) h
  where
    go blks 0 =
        pure $ Blockchain blks
    go blks n = do
        blk <- arbitraryValidNakamotoBlock blks
        go (blk <| blks) (n - 1)

arbitraryValidNakamotoBlock
    :: forall tx. (Arbitrary tx, Serialise tx)
    => NonEmpty (Block tx Nakamoto.PoW)
    -> Gen (Block tx Nakamoto.PoW)
arbitraryValidNakamotoBlock prefix@(parent:|_) | prevHeader <- blockHeader parent = do
    txs        <- arbitrary :: Gen [tx]
    elapsed    <- choose (60 * seconds, 120 * seconds)
    blockState <- arbitrary :: Gen Word8
    blockSeal  <- arbitrary
    blockDiffi <- pure $ Nakamoto.chainDifficulty (toList prefix)
    let header = emptyHeader
               { blockPrevHash   = headerHash prevHeader
               , blockDataHash   = hashTxs txs
               , blockStateHash  = hashState blockState
               , blockSeal
               , blockTimestamp  = blockTimestamp prevHeader `timeAdd` elapsed
               , blockDifficulty = blockDiffi
               }
    pure $ mkBlock header txs

arbitraryBlockchain :: (Arbitrary tx, Serialise tx) => Gen (Blockchain tx ())
arbitraryBlockchain = do
    chain <- pure $ fromGenesis $ emptyGenesisBlock epoch
    rest <- arbitraryValidBlock chain
    pure $ rest |> chain

arbitraryBlock :: forall tx s. (Serialise s, Serialise tx, Arbitrary tx, Arbitrary s) => Gen (Block tx s)
arbitraryBlock =
    arbitraryBlockWith =<< arbitrary

arbitraryBlockWith :: forall tx s. (Serialise s, Serialise tx, Arbitrary s) => [tx] -> Gen (Block tx s)
arbitraryBlockWith txs = do
    timestamp <- arbitrary
    diffi <- arbitrary `suchThat` (> 0)
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
arbitraryValidBlock (Blockchain (Block prevHeader _ _ :| _)) = do
    txs <- arbitrary :: Gen [tx]
    arbitraryValidBlockWith prevHeader txs

arbitraryValidBlockWith :: (Serialise tx, Serialise s, Arbitrary s) => BlockHeader s -> [tx] -> Gen (Block tx s)
arbitraryValidBlockWith prevHeader txs = do
    elapsed    <- choose (2750 * seconds, 3250 * seconds)
    blockState <- arbitrary :: Gen Word8
    blockSeal  <- arbitrary
    blockDiffi <- Difficulty <$> choose (1, 3)
    let header = emptyHeader
               { blockPrevHash   = headerHash prevHeader
               , blockDataHash   = hashTxs txs
               , blockStateHash  = hashState blockState
               , blockSeal
               , blockTimestamp  = blockTimestamp prevHeader `timeAdd` elapsed
               , blockDifficulty = blockDifficulty prevHeader + blockDiffi
               }
    pure $ mkBlock header txs

arbitraryValidBlockchain :: (Serialise tx, Serialise s, Arbitrary tx, Arbitrary s) => Gen (Blockchain tx s)
arbitraryValidBlockchain = do
    ts <- arbitrary
    seal <- arbitrary
    arbitraryValidBlockchainFrom (sealBlock seal (emptyGenesisBlock ts))

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
