module Test.Oscoin.Node.Mempool
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash
import           Oscoin.Data.Tx
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Event as Mempool

import           Oscoin.Test.Crypto
import           Oscoin.Test.Util

import           Codec.Serialise (Serialise)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Hedgehog.Extended
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Node.Mempool"
    [ testProperty "prop_subscription" (prop_subscription d)
    , testProperty "prop_insertValidateResult" (prop_insertValidateResult d)
    , testProperty "prop_insertLookup" (prop_insertLookup d)
    ]

-- | Test that inserted transactions are published to all subscribers.
prop_subscription :: forall c. Dict (IsCrypto c) -> Property
prop_subscription Dict = property $ do
    -- Create a new mempool of account transactions.
    mp <- lift (Mempool.newIO @c dummyValidate)

    -- Create some arbitrary transactions.
    txs <- forAll $ Gen.list (Range.linear 0 30) genDummyTx
    let validTxs = filter (isRight . dummyValidate) txs

    -- Subscribe to the mempool with the subscription tokens.
    (chan1, chan2, evs1, evs2) <- lift $ atomically $ do
        chan1 <- Mempool.subscribe mp
        chan2 <- Mempool.subscribe mp

        -- Add the transactions.
        forM_ txs $ Mempool.insert mp

        -- Retrieve all events from the channels.
        evs1 <- Mempool.drainChannel chan1
        evs2 <- Mempool.drainChannel chan2

        pure (chan1, chan2, evs1, evs2)

    -- Verify that they contain the transactions we generated.
    validTxs === txsFromEventList evs1
    validTxs === txsFromEventList evs2

    -- If we try to read again, the channel is empty.
    contents1 <- lift $ atomically (Mempool.drainChannel chan1)
    assert $ null contents1
    contents2 <- lift $ atomically (Mempool.drainChannel chan2)
    assert $ null contents2
  where
    txsFromEventList = foldMap $ \case
        (Mempool.Insert txs) -> txs
        _                    -> []


-- | Test that we can lookup valid transaction that are were inserted
-- and that invalida transactions are not inserted
prop_insertLookup :: forall c. Dict (IsCrypto c) -> Property
prop_insertLookup Dict = property $ do
    mp <- lift (Mempool.newIO @c dummyValidate)

    txs <- forAll $ Gen.list (Range.linear 1 30) genDummyTx
    lift $ atomically $ forM_ txs $ Mempool.insert mp

    tx <- forAll $ Gen.element txs
    case tx of
        DummyTxValid _ -> do
            result <- lift $ atomically $ Mempool.lookup mp (hash tx)
            result === Just tx
        DummyTxInvalid -> do
            result <- lift $ atomically $ Mempool.lookup mp (hash tx)
            result === Nothing


-- | Test that the result of `Mempool.insert` matches the
-- result of the validation.
prop_insertValidateResult :: forall c. Dict (IsCrypto c) -> Property
prop_insertValidateResult Dict = property $ do
    mp <- lift (Mempool.newIO @c dummyValidate)

    tx <- forAll genDummyTx
    insertResult <- lift $ atomically $ Mempool.insert mp tx
    case tx of
        DummyTxValid _ -> insertResult === Right ()
        DummyTxInvalid -> insertResult === Left DummyTxError


-------------------------------------------------
-- DummyTx
-------------------------------------------------

data DummyTx
    = DummyTxValid Int
    | DummyTxInvalid
    deriving (Show, Eq, Generic)

instance Serialise DummyTx

instance Condensed DummyTx where
    condensed (DummyTxValid x) = "Tx(" <> condensed x <> ")"
    condensed DummyTxInvalid   = "InvalidTx"

instance (HasHashing c) => Hashable c DummyTx where
    hash = hashSerial

data DummyTxError = DummyTxError
    deriving (Eq)

instance Condensed DummyTxError where
    condensed DummyTxError = "DummyTxError"


type instance TxValidationError c DummyTx = DummyTxError

-- | Generates valid and invalid 'DummyTx's. Valid 'DummyTx's are ten
-- times more likely.
genDummyTx :: Gen DummyTx
genDummyTx = Gen.frequency
    [ (10, DummyTxValid <$> Gen.enumBounded)
    , (1, pure DummyTxInvalid)
    ]

dummyValidate :: DummyTx -> Either (TxValidationError c DummyTx) ()
dummyValidate (DummyTxValid _) = Right ()
dummyValidate DummyTxInvalid   = Left DummyTxError
