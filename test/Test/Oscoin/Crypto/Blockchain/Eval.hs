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
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Test.QuickCheck (Arbitrary)
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Crypto.Blockchain.Eval"
    [ test_buildBlock d
    ]

test_buildBlock :: forall c. Dict (IsCrypto c) -> TestTree
test_buildBlock d@Dict = testGroup "buildBlock"
    [ testProperty "creates receipt for all transactions" $
        \txs -> let (_, _, receipts) = buildTestBlock d mempty txs
                in map receiptTx receipts === map (Crypto.hash @c) txs
    , testProperty "receipts have block hash" $
        \txs -> let (blk, _, receipts) = buildTestBlock d mempty txs
                in conjoin [ receiptTxBlock receipt === blockHash blk | receipt <- receipts ]
    , testProperty "new state is computed" $
        \txs -> let (_, s, _) = buildTestBlock d mempty txs
                    outputs = [ output | Tx output <- txs ]
                in reverse outputs === s
    , testProperty "receipts include output" $
        \txs -> let (_, _, receipts) = buildTestBlock d mempty txs
                in conjoin [ receiptTxOutput receipt === output | (Tx output, receipt) <- zip txs receipts ]
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

data Tx = Tx Output
    deriving (Eq, Show, Generic)

instance Arbitrary Tx where
    arbitrary = Tx <$> arbitrary

instance Serialise Tx

instance Crypto.HasHashing c => Crypto.Hashable c Tx where
    hash = Crypto.hashSerial

blockEval :: Evaluator c St Tx Output
blockEval _beneficiary [] oldState = ([], oldState)
blockEval beneficiary (Tx output:txs) oldState =
    let (outputs, finalState) = blockEval beneficiary txs (output:oldState)
    in (output:outputs, finalState)


-- | Build block on an empty genesis block with 'eval' as defined
-- above.
buildTestBlock
    :: Dict (IsCrypto c)
    -> St
    -> [Tx]
    -> (Block c Tx Unsealed, St, [Receipt c Tx Output])
buildTestBlock Dict st txs =
    buildBlock blockEval epoch someBeneficiary st txs (someGenesisBlock ())
