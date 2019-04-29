module Test.Oscoin.Data.OscoinTx where

import           Oscoin.Prelude

import           Oscoin.Crypto.Address (Address)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Data.Ledger
import           Oscoin.Data.OscoinTx
import           Oscoin.Test.Crypto
import           Oscoin.Test.Util
import           Test.Oscoin.P2P.Handshake (genKeyPair, genKeyPairPair)

import qualified Crypto.Data.Auth.Tree as WorldState

import           Test.Oscoin.Crypto.Address (genAddress)

import           Test.Hedgehog.Extended
import           Test.Tasty
import           Test.Tasty.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


tests :: forall c. Dict (IsCrypto c) -> TestTree
tests Dict = testGroup "Test.Oscoin.Data.OscoinTx"
    [ testProperty "Transfer balance"        (propTransferBalance @c Dict)
    , testProperty "Apply transaction"       (propApplyTx         @c Dict)
    , testProperty "Multi-message"           (propMultiTransfer   @c Dict)
    ]

propApplyTx :: forall c. Dict (IsCrypto c) -> Property
propApplyTx Dict = property $ do
    (pk, pk') <- publicKeyPair @c

    pk /== pk'

    -- We use an empty transaction message list, as this lets us test
    -- the "generic" part of the transaction processing without worrying about
    -- the messages.
    tx    <- pure (mkTx [])
    ws    <- forAll genWorldState
    alice <- forAll (genAddress pk')
    bal   <- forAll genBalancePositive

    -- Should fail because Alice doesn't have an account
    applyTx tx alice ws ===  Left (ErrKeyNotFound (addressKey alice))

    -- Give Alice an account
    ws' <- evalEither $ creditAccount alice bal ws

    -- Should fail because the transaction nonce is incorrect (should be 0)
    forAll (genNonce 1 maxBound) >>= \nonce ->
        applyTx (tx { txNonce = nonce }) alice ws' ===  Left (ErrInvalidNonce nonce)

    -- Should fail because the transaction fee is too low
    applyTx (tx { txFee = 0 }) alice ws' ===  Left (ErrInvalidFee 0)

    -- Should fail because Alice doesn't have enough balance
    applyTx (tx { txFee = bal + 1 }) alice ws' ===  Left (ErrInsufficientBalance bal)

    -- Generate a valid fee of at most the account balance
    fee <- forAll $ genFee tx bal

    -- Should succeed because Alice can pay the minimum fee
    assertRight $ applyTx (tx { txFee = fee }) alice ws'

propTransferBalance :: forall c. Dict (IsCrypto c) -> Property
propTransferBalance Dict = property $ do
    (alice, bob) <- addressPair @c
    bal          <- forAll $ genBalancePositive
    ws           <- evalEither $   creditAccount alice bal WorldState.empty
                               >>= creditAccount bob maxBound

    -- A transfer that would overflow the account should fail
    forAll (pure $ TxTransfer alice bob 1) >>= \msg ->
        applyTxMessage msg alice ws === Left (ErrOverflow bob)

    -- A transfer of zero balance should always fail
    forAll (pure $ TxTransfer alice bob 0) >>= \msg ->
        applyTxMessage msg alice ws === Left (ErrInvalidTransfer 0)

    -- A transfer from bob to alice
    forAll (genTransfer bob alice (Range.constantFrom 1 1 100)) >>= \msg -> do
        -- Should fail if alice is the author of the transaction
        applyTxMessage msg alice ws === Left (ErrNotAuthorized alice)
        -- Should succeed if bob is the author of this transaction
        (ws', _) <- evalEither $ applyTxMessage msg bob ws
        -- Shouldn't create or destroy coins
        balanceTotal ws' === balanceTotal ws

    -- A transfer from alice to bob of an amount greater than her balance
    forAll (genTransfer alice bob (Range.singleton (bal + 1))) >>= \msg ->
        -- Should fail since alice doesn't have the required balance
        applyTxMessage msg alice ws === Left (ErrInsufficientBalance bal)

propMultiTransfer :: forall c. Dict (IsCrypto c) -> Property
propMultiTransfer Dict = property $ do
    (alice, bob)  <- addressPair @c
    bal           <- forAll genBalancePositive
    ws            <- evalEither $ creditAccount alice bal WorldState.empty

    t0            <- pure (TxTransfer alice bob 0)
    t1            <- pure (TxTransfer alice bob 1)
    t2            <- pure (TxTransfer alice bob 2)

    -- A single invalid message (`t0`) fails the transaction
    invalidTx <- pure (mkTx [t1, t0, t2])
    applyTx (invalidTx { txFee = minimumTxFee invalidTx }) alice ws === Left (ErrInvalidTransfer 0)

    -- Multiple messages combine
    tx <- pure (mkTx [t1, t1, t2])
    (ws', _) <- evalEither $ applyTx (tx { txFee = minimumTxFee tx }) alice ws

    lookupBalance bob ws' === Right 4

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

lookupBalance :: forall c. (IsCrypto c) => Address c -> WorldState c -> Either (TxError c) Balance
lookupBalance addr ws = accountBalance <$> lookupAccount addr ws

publicKey :: forall c. (IsCrypto c) => PropertyT IO (PublicKey c)
publicKey = fst <$> genKeyPair

publicKeyPair :: forall c. (IsCrypto c) => PropertyT IO (PublicKey c, PublicKey c)
publicKeyPair = do
    ((a, _), (b, _)) <- genKeyPairPair @c
    pure (a, b)

addressPair :: forall c. (IsCrypto c) => PropertyT IO (Address c, Address c)
addressPair = do
    ((a, _), (b, _)) <- genKeyPairPair @c
    alice            <- forAll $ genAddress a
    bob              <- forAll $ genAddress b
    pure (alice, bob)

assertRight :: Condensed a => Either a b -> PropertyT IO ()
assertRight = void . evalEither

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

genFee :: Tx c -> Balance -> Gen Balance
genFee tx maxFee = Gen.integral (Range.constantFrom minFee minFee maxFee)
  where
    minFee = minimumTxFee tx

genNonce :: Nonce -> Nonce -> Gen Nonce
genNonce lower upper = Gen.integral (Range.constantFrom lower lower upper)

genBalance :: Gen Balance
genBalance = Gen.integral Range.constantBounded

genBalancePositive :: Gen Balance
genBalancePositive = Gen.integral (Range.constantFrom 1 1 maxBound)

genHash :: forall c. IsCrypto c => Gen (Crypto.Hash c)
genHash = do
    g <- Gen.enumBounded :: Gen Word32
    pure $ Crypto.fromHashed $ Crypto.hashBinary @c g

genContributions :: Gen [Contribution c]
genContributions = pure []

genDependencyUpdates :: Gen [DependencyUpdate c]
genDependencyUpdates = pure []

genAccount :: Address c -> Gen (Account c)
genAccount addr = Account addr <$> genBalance <*> genNonce 0 maxBound

genTx :: IsCrypto c => PublicKey c -> Gen (Tx c)
genTx pk = mkTx . pure <$> genTxMessage pk

genWorldState :: Gen (WorldState c)
genWorldState = pure WorldState.empty

genTxMessage :: IsCrypto c => PublicKey c -> Gen (TxMessage c)
genTxMessage pk = Gen.choice
    [ genRegisterProject pk
    , genUnregisterProject pk
    , genAuthorize pk
    , genDeauthorize pk
    , genCheckpoint pk
    , genUpdateContract pk
    ]

genRegisterProject :: PublicKey c -> Gen (TxMessage c)
genRegisterProject pk = TxRegisterProject <$> genAddress pk

genUnregisterProject :: PublicKey c -> Gen (TxMessage c)
genUnregisterProject pk = TxUnregisterProject <$> genAddress pk

genAuthorize :: PublicKey c -> Gen (TxMessage c)
genAuthorize pk = TxAuthorize <$> genAddress pk <*> genAddress pk

genDeauthorize :: PublicKey c -> Gen (TxMessage c)
genDeauthorize pk = TxAuthorize <$> genAddress pk <*> genAddress pk

genCheckpoint :: IsCrypto c => PublicKey c -> Gen (TxMessage c)
genCheckpoint pk =
    TxCheckpoint
        <$> genAddress pk
        <*> genHash
        <*> genContributions
        <*> genDependencyUpdates

genUpdateContract :: PublicKey c -> Gen (TxMessage c)
genUpdateContract pk = TxUpdateContract <$> genAddress pk

genTransfer :: Address c -> Address c -> Range Balance -> Gen (TxMessage c)
genTransfer a b range =
    TxTransfer a b <$> Gen.integral range

