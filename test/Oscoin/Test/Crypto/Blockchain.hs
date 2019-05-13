module Oscoin.Test.Crypto.Blockchain
    ( testBlockchain
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus
import qualified Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain (showChainDigest)
import           Oscoin.Crypto.Blockchain.Block

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Blockchain.Generators

import qualified Codec.Serialise as Serialise
import           Data.Binary.Put (putWord32le, runPut)
import qualified Data.ByteString.Lazy as LBS

import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Text ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

testBlockchain
    :: forall c. Dict (IsCrypto c)
    -> Consensus.Config
    -> TestTree
testBlockchain d@Dict config = testGroup "Blockchain"
    [ testValidateBlock d config
    , testProperty "Block: deserialise . serialise == id" $
        \(blk :: Block c ByteString ByteString) ->
            (Serialise.deserialise . Serialise.serialise) blk == blk
    , testProperty "BlockData: deserialise . serialise == id" $
        \(blkData :: BlockData c ByteString) ->
            (Serialise.deserialise . Serialise.serialise) blkData == blkData
    , testProperty "Difficulty: parseDifficulty . prettyDifficulty == id" $
        forAll genDifficulty $ \(diffi :: Difficulty) ->
            (parseDifficulty. prettyDifficulty) diffi == Just diffi
    , testProperty "Difficulty (compact): decode . encode == id" $
        \(diffi :: Integer) -> -- TODO(alexis): Should only create 256-bit integers.
            ( decodeDifficulty
            . encodeDifficulty
            ) diffi == (diffi, False)
    , testProperty "Difficulty (compact): Word32 encoding" $
        \(w :: Word32) ->
            ( readWord32LE
            . LBS.toStrict
            . runPut
            . putWord32le
            ) w == Just w
    , testCase "Difficulty (compact): matches Bitcoin format" $ do
        decodeDifficulty (unsafeDifficulty 0x1b0404cb) @?=
            (0x404CB000000000000000000000000000000000000000000000000, False)
        decodeDifficulty (unsafeDifficulty 0x1d00ffff) @?=
            (0xFFFF0000000000000000000000000000000000000000000000000000, False)
    ]

testValidateBlock
    :: forall c. Dict (IsCrypto c)
    -> Consensus.Config
    -> TestTree
testValidateBlock Dict config = testGroup "Nakamoto: validateBlockchain"
    [ testProperty "Valid blockchains validate" $
        forAllShow (genNakamotoBlockchain @c @Text) (toS . showChainDigest) $
            \blks -> let result = validateBlockchain Nakamoto.validateFull blks
                     in counterexample (show result) (result == Right ())
    , testProperty "Blocks bigger than the maximum size won't validate" $
        forAll (arbitrary @(Block c Text ())) $
          \block -> let result = validateBlockSize config { Consensus.maxBlockSize = 1 } block
                    in counterexample (show result) (hasExceededMaxSize result)
    ]

hasExceededMaxSize :: Either (ValidationError c) a -> Bool
hasExceededMaxSize (Left (InvalidBlockSize _)) = True
hasExceededMaxSize _                           = False
