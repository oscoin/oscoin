module Test.Oscoin.Storage.Ledger
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Blockchain.Eval
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.Block.Memory (newBlockStoreFromGenesisIO)
import qualified Oscoin.Storage.Ledger as Ledger
import qualified Oscoin.Time as Time

import           Codec.Serialise

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary ()
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Crypto.Blockchain.Block.Helpers
                 (defaultBeneficiary)
import           Test.Oscoin.DummyLedger
import           Test.QuickCheck (Arbitrary)
import           Test.Tasty
import           Test.Tasty.Hedgehog

type DummySeal = ()

tests :: Dict (IsCrypto c) -> TestTree
tests c = testGroup "Test.Oscoin.Storage.Ledger"
    [ testProperty "prop_getTipWithState" $ prop_getTipWithState c
    , testProperty "prop_lookupReceipt" $ prop_lookupReceipt c
    , testProperty "prop_buildNextBlock" $ prop_buildNextBlock c
    ]

-- | After adding a block to the block store 'Ledger.getTipWithState'
-- returns the added block and the evaluated state.
prop_getTipWithState :: forall c. Dict (IsCrypto c) -> Property
prop_getTipWithState Dict = property $ do
    (ledger, blockStoreWriter) <- newDummyLedger @c
    (parentBlock, parentState) <- evalEither =<< Ledger.getTipWithState ledger
    (blk, st) <- forAll $ genEvaledBlock parentBlock parentState dummyEvalBlock
    BlockStore.insertBlock blockStoreWriter blk
    result <- Ledger.getTipWithState ledger
    result === Right (blk, st)


-- | After adding a block to the block store 'Ledger.lookupReceipt' returns the
-- receipts for transactions in the block
prop_lookupReceipt :: forall c. Dict (IsCrypto c) -> Property
prop_lookupReceipt Dict = property $ do
    (ledger, blockStoreWriter) <- newDummyLedger @c
    (parentBlock, parentState) <- evalEither =<< Ledger.getTipWithState ledger
    (blk, _) <- forAll $ genEvaledBlock parentBlock parentState dummyEvalBlock
    BlockStore.insertBlock blockStoreWriter blk
    tx <- forAll $ Gen.element (toList $ blockTxs blk)
    maybeReceipt <- Ledger.lookupReceipt ledger (Crypto.hash tx)
    receipt <- case maybeReceipt of
        Nothing      -> failure
        Just receipt -> pure receipt

    receiptTx receipt === Crypto.hash tx
    receiptTxBlock receipt === blockHash blk
    receiptTxOutput receipt === Right tx


-- Checks that a block created with 'Ledger.buildNextBlock' satisfies
-- the following properties:
--
-- * The block includes the proposed transactions
-- * The state hash is correctly computed
-- * The 'parentHash' of the block points to the tip of the blockchain.
--
prop_buildNextBlock :: forall c. Dict (IsCrypto c) -> Property
prop_buildNextBlock Dict = property $ do
    (ledger, _) <- newDummyLedger @c
    (parentBlock, parentState) <- evalEither =<< Ledger.getTipWithState ledger
    txs <- forAll $ Gen.list (Range.linear 1 6) Gen.arbitrary
    timestamp <- forAll $ Gen.arbitrary
    nextBlock <- evalEither =<< Ledger.buildNextBlock ledger timestamp defaultBeneficiary txs

    let (_, expectedState) = dummyEvalBlock (defaultBeneficiary @c) txs parentState
    toList (blockTxs nextBlock) === txs
    blockStateHash (blockHeader nextBlock) === Crypto.fromHashed (Crypto.hash expectedState)
    parentHash nextBlock === blockHash parentBlock


-- | Creates a new 'Ledger' and 'BlockStoreWriter' pair with a single
-- genesis block already in the block store.
newDummyLedger
    :: (IsCrypto crypto, MonadIO m)
    => m
        ( Ledger.Ledger crypto DummySeal DummyTx DummyOutput DummyState m
        , BlockStore.BlockStoreWriter crypto DummyTx DummySeal m
        )
newDummyLedger = do
    (blockStoreReader, blockStoreWriter) <- liftIO $ newBlockStoreFromGenesisIO genesisBlk
    ledger <- liftIO $ Ledger.newFromBlockStoreIO dummyEvalBlock blockStoreReader genesisState
    pure (ledger, BlockStore.hoistBlockStoreWriter liftIO blockStoreWriter)
  where
    genesisState = [] :: [DummyTx]
    genesisBlk = sealBlock () $ emptyGenesisFromState Time.epoch defaultBeneficiary genesisState


-- | Generate an arbitrary child block with arbitrary transactions and
-- with a correct state hash.
genEvaledBlock
    :: ( IsCrypto c
       , Crypto.Hashable c state
       , MonadGen m
       , Serialise s
       , Serialise tx
       , Arbitrary s
       , Arbitrary tx
       )
    => Block c tx s
    -> state
    -> Evaluator c state tx output
    -> m (Block c tx s, state)
genEvaledBlock parentBlock parentState evl = do
    txs <- Gen.list (Range.linear 1 6) Gen.arbitrary
    let (_, newState) = evl defaultBeneficiary txs parentState
    newBlock <- Gen.quickcheck $ genBlockWith parentBlock txs newState
    pure (newBlock, newState)
