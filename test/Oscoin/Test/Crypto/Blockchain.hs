module Oscoin.Test.Crypto.Blockchain
    ( testBlockchain
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Evaluator
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Blockchain.Eval
import           Oscoin.Crypto.Hash (Hashable(..), hashSerial)
import           Oscoin.Time

import           Codec.Serialise (Serialise)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck


testBlockchain :: TestTree
testBlockchain = testGroup "Blockchain.Eval"
    [ testBuildBlock
    ]

testBuildBlock :: TestTree
testBuildBlock = testGroup "buildBlock"
    [ testProperty "creates receipt for all transactions" $
        \txs -> let (_, receipts) = buildTestBlock txs
                in map receiptTx receipts === map hash txs
    , testProperty "receipts have block hash" $
        \txs -> let (blk, receipts) = buildTestBlock txs
                in conjoin [ receiptTxBlock receipt === blockHash blk | receipt <- receipts ]
    , testProperty "valid transactions create new state" $
        \txs -> let (blk, _) = buildTestBlock txs
                    validTxOutputs = [ output | TxOk output <- txs ]
                in reverse validTxOutputs === blockState (blockHeader blk)
    , testProperty "only valid transactions are included in block" $
        \txs -> let (blk, _) = buildTestBlock txs
                    validTxs = filter txIsOk txs
                in validTxs === toList (blockData blk)
    , testProperty "transactions errors recorded in receipts" $
        \txs err -> let (_, receipts) = buildTestBlock txsWithError
                        txsWithError = TxErr err : txs
                    in (receiptTxOutput <$> head receipts) === Just (Left (EvalError (show err)))
    , testProperty "error transactions do not change block" $
        \txs -> let validTxs = [ TxOk out | TxOk out <- txs ]
                    (blkWithErrors, _) = buildTestBlock txs
                    (blkWithoutErrors, _) = buildTestBlock validTxs
                in  blockData blkWithErrors === blockData blkWithoutErrors
    ]

--
-- * Test evaluator
--
-- We define a test evaluator where a transaction is either an output
-- or an error and the state is just the list of outputs.
--

type Output = Int

type St = [Output]

data Tx
    = TxOk Output
    | TxErr Int
    deriving (Eq, Show, Generic)

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
buildTestBlock :: [Tx] -> (Block Tx St, [Receipt Tx Output])
buildTestBlock txs = buildBlock eval epoch txs (emptyGenesisBlock epoch [])
