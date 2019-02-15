module Oscoin.Test.Crypto.Blockchain
    ( testBlockchain
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus
import qualified Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain (showChainDigest)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Blockchain.Eval
import           Oscoin.Crypto.Hash
                 (Hashable(..), Hashed, fromHashed, hashSerial, toHashed)
import           Oscoin.Time

import           Oscoin.Test.Crypto.Blockchain.Arbitrary

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import           Data.Binary.Put (putWord32le, runPut)
import qualified Data.ByteString.Lazy as LBS

import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Text ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

testBlockchain :: Consensus.Config -> TestTree
testBlockchain config = testGroup "Blockchain"
    [ testBuildBlock
    , testValidateBlock config
    , testProperty "JSON Receipt" $ do
        receipt <- arbitraryReceipt
        pure $ (Aeson.decode . Aeson.encode) receipt == Just receipt
    , testProperty "Block: deserialise . serialise == id" $
        \(blk :: Block ByteString ByteString) ->
            (Serialise.deserialise . Serialise.serialise) blk == blk
    , testProperty "Block: decode . encode == id (JSON)" $
        \(blk :: Block String String) ->
            (Aeson.decode . Aeson.encode) blk == Just blk
    , testProperty "Difficulty: decode . encode == id (JSON)" $
        \(d :: Difficulty) ->
            (Aeson.decode . Aeson.encode) d == Just d
    , testProperty "Difficulty: parseDifficulty . prettyDifficulty == id" $
        \(d :: Difficulty) ->
            (parseDifficulty. prettyDifficulty) d == Just d
    , testProperty "Difficulty (compact): decode . encode == id" $
        \(d :: Integer) -> -- TODO(alexis): Should only create 256-bit integers.
            ( decodeDifficulty
            . encodeDifficulty
            ) d == (d, False)
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

testValidateBlock :: Consensus.Config -> TestTree
testValidateBlock config = testGroup "Nakamoto: validateBlockchain"
    [ testProperty "Valid blockchains validate" $
        forAllShow (arbitraryNakamotoBlockchain @Text) (toS . showChainDigest) $
            \blks -> let result = validateBlockchain Nakamoto.validateBlock blks
                     in counterexample (show result) (result == Right ())
    , testProperty "Blocks bigger than the maximum size won't validate" $
        forAll (arbitrary @(Block Text Nakamoto.PoW)) $
          \block -> let result = validateBlockSize config { Consensus.maxBlockSize = 1 } block
                    in counterexample (show result) (hasExceededMaxSize result)
    ]

hasExceededMaxSize :: Either ValidationError a -> Bool
hasExceededMaxSize (Left (InvalidBlockSize _)) = True
hasExceededMaxSize _                           = False

testBuildBlock :: TestTree
testBuildBlock = testGroup "buildBlock"
    [ testProperty "creates receipt for all transactions" $
        \txs -> let (_, _, receipts) = buildTestBlock mempty txs
                in map receiptTx receipts === map hash txs
    , testProperty "receipts have block hash" $
        \txs -> let (blk, _, receipts) = buildTestBlock mempty txs
                in conjoin [ receiptTxBlock receipt === blockHash blk | receipt <- receipts ]
    , testProperty "valid transactions create new state" $
        \txs -> let (_, s, _) = buildTestBlock mempty txs
                    validTxOutputs = [ output | TxOk output <- txs ]
                in reverse validTxOutputs === s
    , testProperty "only valid transactions are included in block" $
        \txs -> let (blk, _, _) = buildTestBlock mempty txs
                    validTxs = filter txIsOk txs
                in validTxs === toList (blockData blk)
    , testProperty "transactions errors recorded in receipts" $
        \txs err -> let (_, _, receipts) = buildTestBlock mempty txsWithError
                        txsWithError = TxErr err : txs
                    in (receiptTxOutput <$> head receipts) === Just (Left (EvalError (show err)))
    , testProperty "error transactions do not change block" $
        \txs -> let validTxs = [ TxOk out | TxOk out <- txs ]
                    (blkWithErrors, _, _) = buildTestBlock mempty txs
                    (blkWithoutErrors, _, _) = buildTestBlock mempty validTxs
                in  blockData blkWithErrors === blockData blkWithoutErrors
    ]


arbitraryReceipt :: Gen (Receipt Tx Output)
arbitraryReceipt = do
    receiptTxBlock <- fromHashed <$> arbitraryHashed
    receiptTx <- arbitraryHashed
    receiptTxOutput <- liftArbitrary2 (EvalError <$> arbitrary) arbitrary
    pure Receipt{..}
  where
    arbitraryHashed :: Gen (Hashed a)
    arbitraryHashed = toHashed . fromHashed . hash <$> (arbitrary :: Gen ByteString)


--
-- * Test evaluator
--
-- We define a test evaluator where a transaction is either an output
-- or an error and the state is just the list of outputs.
--

type Output = Word8

type St = [Output]

data Tx
    = TxOk Output
    | TxErr Int
    deriving (Eq, Show, Generic)

instance Aeson.ToJSON Tx
instance Aeson.FromJSON Tx

txIsOk :: Tx -> Bool
txIsOk (TxOk _)  = True
txIsOk (TxErr _) = False

instance Arbitrary Tx where
    arbitrary = txFromEither <$> arbitrary
      where
        txFromEither (Left err)     = TxErr err
        txFromEither (Right output) = TxOk output

instance Serialise Tx

instance Hashable Tx where
    hash = hashSerial

eval :: Evaluator St Tx Output
eval (TxOk output) st = Right (output, output : st)
eval (TxErr err) _    = Left (EvalError (show err))


-- | Build block on an empty genesis block with 'eval' as defined
-- above.
buildTestBlock :: St -> [Tx] -> (Block Tx (), St, [Receipt Tx Output])
buildTestBlock st txs = buildBlock eval epoch st txs (blockHash $ emptyGenesisBlock epoch)
