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

import           Oscoin.Test.Crypto
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

testBlockchain
    :: forall c. Dict (IsCrypto c)
    -> Consensus.Config
    -> TestTree
testBlockchain d@Dict config = testGroup "Blockchain"
    [ testBuildBlock d
    , testValidateBlock d config
    , testProperty "JSON Receipt" $ do
        receipt <- arbitraryReceipt d
        pure $ (Aeson.decode . Aeson.encode) receipt == Just receipt
    , testProperty "Block: deserialise . serialise == id" $
        \(blk :: Block c ByteString ByteString) ->
            (Serialise.deserialise . Serialise.serialise) blk == blk
    , testProperty "Block: decode . encode == id (JSON)" $
        \(blk :: Block c String String) ->
            (Aeson.decode . Aeson.encode) blk == Just blk
    , testProperty "Difficulty: decode . encode == id (JSON)" $
        \(diffi :: Difficulty) ->
            (Aeson.decode . Aeson.encode) diffi == Just diffi
    , testProperty "Difficulty: parseDifficulty . prettyDifficulty == id" $
        \(diffi :: Difficulty) ->
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
        forAllShow (arbitraryNakamotoBlockchain @c @Text) (toS . showChainDigest) $
            \blks -> let result = validateBlockchain Nakamoto.validateFull blks
                     in counterexample (show result) (result == Right ())
    , testProperty "Blocks bigger than the maximum size won't validate" $
        forAll (arbitrary @(Block c Text Nakamoto.PoW)) $
          \block -> let result = validateBlockSize config { Consensus.maxBlockSize = 1 } block
                    in counterexample (show result) (hasExceededMaxSize result)
    ]

hasExceededMaxSize :: Either (ValidationError c) a -> Bool
hasExceededMaxSize (Left (InvalidBlockSize _)) = True
hasExceededMaxSize _                           = False

testBuildBlock
    :: forall c. Dict (IsCrypto c)
    -> TestTree
testBuildBlock d@Dict = testGroup "buildBlock"
    [ testProperty "creates receipt for all transactions" $
        \txs -> let (_, _, receipts) = buildTestBlock d mempty txs
                in map receiptTx receipts === map (hash @c) txs
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


arbitraryReceipt
    :: forall c. Dict (IsCrypto c)
    -> Gen (Receipt c Tx Output)
arbitraryReceipt Dict = do
    receiptTxBlock <- fromHashed <$> arbitraryHashed
    receiptTx <- arbitraryHashed
    receiptTxOutput <- liftArbitrary2 (EvalError <$> arbitrary) arbitrary
    pure Receipt{..}
  where
    arbitraryHashed :: Gen (Hashed c a)
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

instance HasHashing c => Hashable c Tx where
    hash = hashSerial

eval :: Evaluator St Tx Output
eval (TxOk output) st = Right (output, output : st)
eval (TxErr err) _    = Left (EvalError (show err))


-- | Build block on an empty genesis block with 'eval' as defined
-- above.
buildTestBlock
    :: Dict (IsCrypto c)
    -> St
    -> [Tx]
    -> (Block c Tx Unsealed, St, [Receipt c Tx Output])
buildTestBlock Dict st txs =
    buildBlock eval epoch st txs (blockHash $ emptyGenesisBlock epoch)
