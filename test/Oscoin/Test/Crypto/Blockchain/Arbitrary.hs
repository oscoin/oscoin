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

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Generators
                 (genBlockchainFrom, genNakamotoBlockchainFrom)
import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

arbitraryNakamotoGenesis
    :: forall c tx.
       ( IsCrypto c
       , Serialise tx
       , Arbitrary tx
       )
    => Gen (Block c tx (Sealed c Nakamoto.PoW))
arbitraryNakamotoGenesis = do
    txs  <- arbitrary
    time <- arbitrary
    case buildGenesis identityEval time txs (Rad.pureEnv @c) of
        Left _    -> panic "Failed to generate arbitrary genesis block"
        Right blk -> do
            result <- mineGenesis
                (Nakamoto.mineNakamoto (const Nakamoto.minDifficulty)) blk
            case result of
                Left _     -> panic "Failed to generate arbirary genesis block"
                Right sblk -> pure sblk

arbitraryBlockchain
    :: ( IsCrypto c
       , Serialise tx
       , Serialise s
       , Arbitrary tx
       , Arbitrary s
       )
    => Gen (Blockchain c tx s)
arbitraryBlockchain = do
    ts <- arbitrary
    seal <- arbitrary
    genBlockchainFrom (sealBlock seal (emptyGenesisBlock ts))

arbitraryNakamotoBlockchain
    :: (IsCrypto c, Serialise tx, Arbitrary tx)
    => Gen (Blockchain c tx Nakamoto.PoW)
arbitraryNakamotoBlockchain = do
    gen <- arbitraryNakamotoGenesis
    genNakamotoBlockchainFrom gen
