{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Crypto.Blockchain.Arbitrary
    ( arbitraryBlockchain
    , arbitraryNakamotoBlockchain
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Mining (mineGenesis)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval
import qualified Oscoin.Data.RadicleTx as Rad

import           Codec.Serialise (Serialise)

import           Oscoin.Test.Crypto.Blockchain.Generators
                 (genBlockchainFrom, genNakamotoBlockchainFrom)
import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

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

arbitraryBlockchain
    :: (Serialise tx, Serialise s, Arbitrary tx, Arbitrary s)
    => Gen (Blockchain tx s)
arbitraryBlockchain = do
    ts <- arbitrary
    seal <- arbitrary
    genBlockchainFrom (sealBlock seal (emptyGenesisBlock ts))

arbitraryNakamotoBlockchain
    :: (Serialise tx, Arbitrary tx)
    => Gen (Blockchain tx Nakamoto.PoW)
arbitraryNakamotoBlockchain = do
    gen <- arbitraryNakamotoGenesis
    genNakamotoBlockchainFrom gen
