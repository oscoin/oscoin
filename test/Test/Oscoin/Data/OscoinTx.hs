module Test.Oscoin.Data.OscoinTx (tests, genTx) where

import           Oscoin.Prelude

import           Oscoin.Configuration (allNetworks)
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Data.Ledger
import           Oscoin.Data.OscoinTx
import           Oscoin.Test.Crypto
import           Oscoin.Test.Util

import qualified Codec.Serialise as CBOR
import qualified Crypto.Data.Auth.Tree as WorldState

import           Numeric.Natural

import           Test.Oscoin.Crypto.Hash.Gen (genHash, genShortHash)
import           Test.Oscoin.Crypto.PubKey.Gen (genPublicKey, genSignature)

import           Test.Hedgehog.Extended
import           Test.Tasty
import           Test.Tasty.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


tests
    :: forall c. CBOR.Serialise (Contribution c)
    => CBOR.Serialise (DependencyUpdate c)
    => Dict (IsCrypto c) -> TestTree
tests Dict = testGroup "Test.Oscoin.Data.OscoinTx"
    [ testProperty "Transfer balance"        (propTransferBalance     @c Dict)
    , testProperty "Apply transaction"       (propApplyTxPayload      @c Dict)
    , testProperty "Multi-message"           (propMultiTransfer       @c Dict)
    , testProperty "Tx fee & burn"           (propTxFeeBurn           @c Dict)
    , testProperty "Register project"        (propRegisterProj        @c Dict)
    , testProperty "Nonces"                  (propNonce               @c Dict)
    , testProperty "Roundtrip Tx"            (propRoundtripTx         @c Dict)
    , testProperty "Roundtrip StateVal"      (propRoundtripStateVal   @c Dict)
    ]

propApplyTxPayload :: forall c. Dict (IsCrypto c) -> Property
propApplyTxPayload Dict = property $ do
    alicePk <- forAll (genPublicKey @c)
    charlie <- forAll (genAccountId @c)

    let alice = toAccountId alicePk

    -- We use an empty transaction message list, as this lets us test
    -- the "generic" part of the transaction processing without worrying about
    -- the messages.
    tx    <- pure (mkTxPayload [] alicePk)
    ws    <- forAll genWorldState
    bal   <- forAll genBalancePositive

    -- Should fail because Alice doesn't have an account
    applyTxPayload tx charlie ws === Left (ErrKeyNotFound (accountKey alice))

    -- Give Alice an account
    ws' <- evalEither $ creditAccount alice bal ws

    -- Should fail because the transaction nonce is incorrect (should be 0)
    forAll (genNonce 1 maxBound) >>= \nonce ->
        applyTxPayload (tx { txNonce = nonce }) charlie ws' ===  Left (ErrInvalidNonce nonce)

    -- Should fail because the transaction fee is too low
    applyTxPayload (tx { txFee = 0 }) charlie ws' ===  Left (ErrInvalidFee 0)

    -- Should fail because Alice doesn't have enough balance
    applyTxPayload (tx { txFee = bal + 1 }) charlie ws' ===  Left (ErrInsufficientBalance bal)

    -- Generate a valid fee of at most the account balance
    fee <- forAll $ genFee tx bal

    -- Should succeed because Alice can pay the minimum fee
    (ws'', _) <- evalEither $ applyTxPayload (tx { txFee = fee }) charlie ws'

    -- Alice's nonce is now incremented to `1`.
    lookupNonce alice ws'' === Right 1

propTransferBalance :: forall c. Dict (IsCrypto c) -> Property
propTransferBalance Dict = property $ do
    (alice, bob) <- forAll (genAccountIdPair @c)
    bal          <- forAll $ genBalancePositive
    ws           <- evalEither $   creditAccount alice bal WorldState.empty
                               >>= creditAccount bob maxBound

    -- TODO(cloudhead): Test sending to self

    -- A transfer that would overflow the account should fail
    forAll (pure $ TxTransfer bob 1) >>= \msg ->
        applyTxMessage msg alice ws === Left (ErrOverflow bob)

    -- A transfer of zero balance should always fail
    forAll (pure $ TxTransfer bob 0) >>= \msg ->
        applyTxMessage msg alice ws === Left (ErrInvalidTransfer 0)

    -- A transfer to alice
    forAll (genTransfer alice (Range.constantFrom 1 1 100)) >>= \msg -> do
        -- Should fail if alice is the author of the transaction
        applyTxMessage msg alice ws === Left (ErrNotAuthorized alice)
        -- Should succeed if bob is the author of this transaction
        (ws', _) <- evalEither $ applyTxMessage msg bob ws
        -- Shouldn't create or destroy coins
        balanceTotal ws' === balanceTotal ws

    -- A transfer to bob of an amount greater than her balance
    forAll (genTransfer bob (Range.singleton (bal + 1))) >>= \msg ->
        -- Should fail since alice doesn't have the required balance
        applyTxMessage msg alice ws === Left (ErrInsufficientBalance bal)

propMultiTransfer :: forall c. Dict (IsCrypto c) -> Property
propMultiTransfer Dict = property $ do
    alicePk   <- forAll (genPublicKey @c)
    bobPk     <- forAll (genPublicKey @c)
    charlie   <- forAll (genAccountId @c)

    let (alice, bob) = (toAccountId alicePk, toAccountId bobPk)

    bal           <- forAll genBalancePositive
    ws            <- evalEither $ creditAccount alice bal WorldState.empty

    t0            <- pure (TxTransfer bob 0)
    t1            <- pure (TxTransfer bob 1)
    t2            <- pure (TxTransfer bob 2)

    -- A single invalid message (`t0`) fails the transaction
    invalidTx <- pure (mkTxPayload [t1, t0, t2] alicePk)
    applyTxPayload (invalidTx { txFee = minimumTxFee invalidTx }) charlie ws
        === Left (ErrInvalidTransfer 0)

    -- Multiple messages combine
    tx <- pure (mkTxPayload [t1, t1, t2] alicePk)
    (ws', _) <- evalEither $ applyTxPayload (tx { txFee = minimumTxFee tx }) charlie ws

    lookupBalance bob ws' === Right 4

propTxFeeBurn :: forall c. Dict (IsCrypto c) -> Property
propTxFeeBurn Dict = property $ do
    alicePk <- forAll (genPublicKey @c)
    charlie <- forAll (genAccountId @c)

    let alice = toAccountId alicePk

    annotate . condensedS $ (accountKey alice, accountKey charlie)

    tx   <- pure (mkTxPayload [] alicePk)

    -- Generate a valid fee and burn.
    fee  <- forAll $ genBalanceRange (minimumTxFee tx) (minimumTxFee tx * 2)
    burn <- forAll $ genBalanceRange 0 3
    bal  <- forAll $ genBalanceRange (fee + burn + 1) maxBound
    ws   <- evalEither $ creditAccount alice bal WorldState.empty

    annotate . condensedS $ ws

    (ws', _) <- evalEither $ applyTxPayload (tx { txFee = fee, txBurn = burn }) charlie ws

    -- The burn and fee is debited from Alice's account, and the fee is credited to Charlie's.
    lookupBalance alice   ws' === Right (bal - burn - fee)
    lookupBalance charlie ws' === Right fee

propRegisterProj :: forall c. Dict (IsCrypto c) -> Property
propRegisterProj Dict = property $ do
    alicePk <- forAll (genPublicKey @c)
    bobPk   <- forAll (genPublicKey @c)

    let (alice, bob) = (toAccountId alicePk, toAccountId bobPk)

    proj               <- forAll (genAccountId @c)
    bal                <- forAll $ genBalancePositive
    ws                 <- evalEither $ (creditAccount alice bal >=> creditAccount bob 1)
                                       WorldState.empty

    -- Attempting to unregister a non-existing project fails.
    forAll (genUnregisterProject proj) >>= \msg ->
        applyTxMessage msg alice ws === Left (ErrKeyNotFound (accountKey proj))

    -- Registered projects can be looked up.
    forAll (genRegisterProject proj) >>= \msg -> do
        (ws', _) <- evalEither $ applyTxMessage msg alice ws
        project  <- evalEither $ lookupProject proj ws'

        -- The looked-up project is the one we registered.
        proj === projectId project

        -- If Alice attempts to re-register, it fails.
        applyTxMessage msg alice ws' === Left (ErrProjectExists proj)

        -- If Bob tries to unregister, it fails, since he isn't a maintainer.
        applyTxMessage (TxUnregisterProject proj) bob ws' ===
            Left (ErrHandlerFailed (HandlerError "Signer must be a project maintainer"))

        -- If Alice tries to unregister, it succeeds.
        (ws'', _) <- evalEither $ applyTxMessage (TxUnregisterProject proj) alice ws'

        -- And the project is no longer in the state.
        lookupProject proj ws'' === Left (ErrKeyNotFound (accountKey proj))

propNonce :: forall c. Dict (IsCrypto c) -> Property
propNonce Dict = property $ do
    alicePk <- forAll (genPublicKey @c)
    charlie <- forAll (genAccountId @c)

    let alice = toAccountId alicePk

    acc <- forAll (genAccount alice)

    let tx = (mkTxPayload [] alicePk) { txNonce = accountNonce acc }
    let ws = insertAccount acc WorldState.empty

    fee      <- forAll (genFee tx (accountBalance acc))
    (ws', _) <- evalEither $ applyTxPayload (tx { txFee = fee }) charlie ws
    nonce'   <- evalEither $ lookupNonce alice ws'

    -- Alice's nonce is now incremented by `1`.
    nonce' === (accountNonce acc + 1)

propRoundtripTx
    :: forall c. CBOR.Serialise (Contribution c)
    => CBOR.Serialise (DependencyUpdate c)
    => HasHashing c
    => Dict (IsCrypto c) -> Property
propRoundtripTx Dict = property $ do
    msg <- forAll (genTxMessage @c)
    (CBOR.deserialise . CBOR.serialise) msg === msg

    author <- forAll genPublicKey
    pay    <- forAll (genTxPayload @c author)
    (CBOR.deserialise . CBOR.serialise) pay === pay

    tx <- forAll (genTx @c)
    (CBOR.deserialise . CBOR.serialise) tx === tx

propRoundtripStateVal
    :: forall c. HasHashing c
    => Dict (IsCrypto c) -> Property
propRoundtripStateVal Dict = property $ do
    sv <- forAll (genStateVal @c)
    (CBOR.deserialise . CBOR.serialise) sv === sv

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

lookupBalance :: forall c. (IsCrypto c) => AccountId c -> WorldState c -> Either (TxError c) Balance
lookupBalance acc ws = accountBalance <$> lookupAccount acc ws

lookupNonce :: forall c. (IsCrypto c) => AccountId c -> WorldState c -> Either (TxError c) Nonce
lookupNonce acc ws = accountNonce <$> lookupAccount acc ws

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

genStateVal :: (HasHashing c) => Gen (StateVal c)
genStateVal = do
    id <- genAccountId
    Gen.choice
        [ AccountVal <$> genAccount id
        , NatVal     <$> genEpoch
        ]

genEpoch :: Gen Natural
genEpoch =
    Gen.integral (Range.constantFrom 0 0 1024)

genAccountIdPair :: HasHashing c => Gen (AccountId c, AccountId c)
genAccountIdPair = do
    a1 <- genAccountId
    a2 <- genAccountId
    pure (a1, a2)

genAccountId :: HasHashing c => Gen (AccountId c)
genAccountId = genShortHash

genFee :: TxPayload c -> Balance -> Gen Balance
genFee tx maxFee = Gen.integral (Range.constantFrom minFee minFee maxFee)
  where
    minFee = minimumTxFee tx

genNonce :: Nonce -> Nonce -> Gen Nonce
genNonce lower upper = Gen.integral (Range.constantFrom lower lower upper)

genBalance :: Gen Balance
genBalance = Gen.integral Range.constantBounded

genBalancePositive :: Gen Balance
genBalancePositive = Gen.integral (Range.constantFrom 1 1 maxBound)

genBalanceRange :: Balance -> Balance -> Gen Balance
genBalanceRange a b = Gen.integral (Range.constantFrom a a b)

genContributions :: Gen [Contribution c]
genContributions = pure []

genDependencyUpdates :: Gen [DependencyUpdate c]
genDependencyUpdates = pure []

genAccount :: AccountId c -> Gen (Account c)
genAccount id = Account id <$> genBalancePositive <*> genNonce 0 maxBound

genWorldState :: Gen (WorldState c)
genWorldState = pure WorldState.empty

genRegisterProject :: AccountId c -> Gen (TxMessage c)
genRegisterProject = pure . TxRegisterProject

genUnregisterProject :: AccountId c -> Gen (TxMessage c)
genUnregisterProject = pure . TxUnregisterProject

genAuthorize :: AccountId c -> Gen (TxMessage c)
genAuthorize pk = pure $ TxAuthorize pk pk

genDeauthorize :: AccountId c -> Gen (TxMessage c)
genDeauthorize pk = pure $ TxDeauthorize pk pk

genCheckpoint :: IsCrypto c => AccountId c -> Gen (TxMessage c)
genCheckpoint pk =
    TxCheckpoint pk
        <$> genHash
        <*> genContributions
        <*> genDependencyUpdates

genUpdateContract :: AccountId c -> Gen (TxMessage c)
genUpdateContract = pure . TxUpdateContract

genTransfer :: AccountId c -> Range Balance -> Gen (TxMessage c)
genTransfer receiver range =
    TxTransfer receiver <$> Gen.integral range

genTxMessage :: IsCrypto c => Gen (TxMessage c)
genTxMessage = do
    acc <- genAccountId
    Gen.choice
        [ genRegisterProject acc
        , genUnregisterProject acc
        , genAuthorize acc
        , genDeauthorize acc
        , genCheckpoint acc
        , genUpdateContract acc
        , genTransfer acc Range.constantBounded
        ]

genTxPayload :: (IsCrypto c) => PublicKey c -> Gen (TxPayload c)
genTxPayload txAuthor = do
    txMessages <- Gen.list (Range.constant 0 3) genTxMessage
    txNonce    <- genNonce 0 maxBound
    txFee      <- genBalance
    txBurn     <- genBalance

    pure TxPayload{..}

genTx :: forall c. (IsCrypto c) => Gen (Tx c)
genTx = do
    pk          <- genPublicKey
    txPayload   <- genTxPayload @c pk
    txNetwork   <- Gen.element allNetworks
    txSignature <- genSignature

    pure  Tx'{..}
