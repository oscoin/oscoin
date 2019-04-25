module Test.Oscoin.Crypto.Blockchain.Eval
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Blockchain.Eval
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Time

import           Codec.Serialise

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary ()
import           Test.QuickCheck (Arbitrary)
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Crypto.Blockchain.Eval"
    [ test_buildBlock d
    , test_evalBlock d
    ]

test_buildBlock :: forall c. Dict (IsCrypto c) -> TestTree
test_buildBlock d@Dict = testGroup "buildBlock"
    [ testProperty "creates receipt for all transactions" $
        \txs -> let (_, _, receipts) = buildTestBlock d mempty txs
                in map receiptTx receipts === map (Crypto.hash @c) txs
    , testProperty "receipts have block hash" $
        \txs -> let (blk, _, receipts) = buildTestBlock d mempty txs
                in conjoin [ receiptTxBlock receipt === blockHash blk | receipt <- receipts ]
    , testProperty "valid transactions create new state" $
        \txs -> let (_, s, _) = buildTestBlock d mempty txs
                    validTxOutputs = [ output | TxOk output <- txs ]
                in reverse validTxOutputs === s
    , testProperty "only valid transactions are included in block" $
        \txs -> let (blk, _, _) = buildTestBlock d mempty txs
                    validTxs = filter txIsOk txs
                in validTxs === toList (blockData blk)
    , testProperty "transactions errors recorded in receipts" $
        \txs err -> let (_, _, receipts) = buildTestBlock d mempty txsWithError
                        txsWithError = TxErr err : txs
                    in (receiptTxOutput <$> head receipts) === Just (Left (EvalError (show err)))
    , testProperty "error transactions do not change block" $
        \txs -> let validTxs = [ TxOk out | TxOk out <- txs ]
                    (blkWithErrors, _, _) = buildTestBlock d mempty txs
                    (blkWithoutErrors, _, _) = buildTestBlock d mempty validTxs
                in  blockData blkWithErrors === blockData blkWithoutErrors
    ]


test_evalBlock :: forall c. Dict (IsCrypto c) -> TestTree
test_evalBlock Dict = testGroup "evalBlock"
    [ testProperty "produces the correct state" $ property $ do
        txs <- arbitrary
        let blk = mkBlock emptyHeader txs :: Block c Tx Unsealed
        let expectedState = reverse [ output | TxOk output <- txs ]
        let (newSt, _) = evalBlock evalTx [] blk
        pure $ newSt === expectedState
    , testProperty "includes all receipts" $ property $ do
        txs <- arbitrary
        let blk = mkBlock emptyHeader txs :: Block c Tx Unsealed
        let (_, receipts) = evalBlock evalTx [] blk
        pure $ conjoin
            [ map receiptTx receipts === map Crypto.hash txs
            , map receiptTxOutput receipts === map txToResult txs
            , map receiptTxBlock receipts === map (\_ -> blockHash blk) txs
            ]
    ]

--
-- * Test evaluator
--
-- We define a test evaluator where a transaction is either an output
-- or an error and the state is just the list of outputs starting with
-- the most recent output.
--

type Output = Word8

type St = [Output]

data Tx
    = TxOk Output
    | TxErr Int
    deriving (Eq, Show, Generic)

txIsOk :: Tx -> Bool
txIsOk (TxOk _)  = True
txIsOk (TxErr _) = False

txToResult :: Tx -> Either EvalError Output
txToResult (TxOk output) = Right output
txToResult (TxErr err)   = Left (EvalError $ show err)

instance Arbitrary Tx where
    arbitrary = txFromEither <$> arbitrary
      where
        txFromEither (Left err)     = TxErr err
        txFromEither (Right output) = TxOk output

instance Serialise Tx

instance Crypto.HasHashing c => Crypto.Hashable c Tx where
    hash = Crypto.hashSerial

evalTx :: Evaluator St Tx Output
evalTx (TxOk output) st = Right (output, output : st)
evalTx (TxErr err) _    = Left (EvalError (show err))


-- | Build block on an empty genesis block with 'eval' as defined
-- above.
buildTestBlock
    :: Dict (IsCrypto c)
    -> St
    -> [Tx]
    -> (Block c Tx Unsealed, St, [Receipt c Tx Output])
buildTestBlock Dict st txs =
    buildBlock evalTx epoch st txs (emptyGenesisBlock epoch)
