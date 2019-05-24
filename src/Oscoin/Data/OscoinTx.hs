{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | The Transaction.
--
module Oscoin.Data.OscoinTx where

import           Oscoin.Prelude hiding (length)

import           Oscoin.Configuration (Network)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey
import           Oscoin.Data.Ledger

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import           Control.Monad.Fail (fail)
import qualified Crypto.Data.Auth.Tree as WorldState
import qualified Data.Aeson as JSON
import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Tx
-------------------------------------------------------------------------------

-- | A transaction on the ledger with the default version.
type Tx c = Tx' 1 c

-- | A signed transaction with parametric version.
data Tx' (version :: Nat) c = Tx'
    { txPayload   :: TxPayload c
    -- ^ The transaction payload.
    , txNetwork   :: Network
    -- ^ The network this transaction belongs to.
    , txSignature :: Signature c
    -- ^ The transaction signature.
    }

-- | Transaction version.
txVersion :: forall version c. KnownNat version => Tx' version c -> Word16
txVersion _ = fromIntegral (natVal (Proxy @version))

data TxValidationError = TxInvalidSignature

txValidate :: Tx c -> Either TxValidationError ()
txValidate _ = Right ()

-- | Transaction payload.
data TxPayload c = TxPayload
    { txMessages :: [TxMessage c]
    -- ^ Transaction message.
    , txNonce    :: Word64
    -- ^ Account nonce.
    , txFee      :: Balance
    -- ^ Transaction fee to miner.
    , txBurn     :: Balance
    -- ^ Transaction burn.
    , txAuthor   :: PublicKey c
    -- ^ The transaction author.
    } deriving (Generic)

-- | A transaction message.
data TxMessage c =
      TxRegisterProject   (AccountId c)
    -- ^ Register a project.
    | TxUnregisterProject (AccountId c)
    -- ^ Unregister a project.
    | TxAuthorize         (AccountId c) (AccountId c)
    -- ^ Authorize an account to operate on a project.
    | TxDeauthorize       (AccountId c) (AccountId c)
    -- ^ De-authorize an account to operate on a project.
    | TxCheckpoint        (AccountId c) (Crypto.Hash c) [Contribution c] [DependencyUpdate c]
    -- ^ Checkpoint a project's state.
    | TxUpdateContract    (AccountId c)
    -- ^ Update a project's contract.
    | TxTransfer          (AccountId c) Balance
    -- ^ Transfer balance between two accounts.
    deriving (Generic)

-- | Transaction output, included in the receipts tree. (Placeholder)
type TxOutput = [TxMessageOutput]

-- | Transaction message output, included in 'TxOutput'.
data TxMessageOutput = TxMessageOutput
    deriving (Show, Eq, Generic)

instance CBOR.Serialise TxMessageOutput

instance JSON.ToJSON   TxMessageOutput
instance JSON.FromJSON TxMessageOutput

instance ByteArrayAccess TxOutput where
    length = length . LBS.toStrict . CBOR.serialise
    withByteArray ba = withByteArray (LBS.toStrict . CBOR.serialise $ ba)

-- | A transaction evaluation error.
data TxError c =
      ErrInsufficientBalance         Balance
    -- ^ Insufficient balance in the account to execute transfer.
    | ErrNotAuthorized               (AccountId c)
    -- ^ This account is not authorized to execute the operation.
    | ErrInvalidTransfer             Balance
    -- ^ A balance transfer of this amount is not valid.
    | ErrKeyNotFound                 StateKey
    -- ^ The key was not found in the state.
    | ErrTypeMismatch                StateKey
    -- ^ The value under this key does not have the expected type.
    | ErrProjectExists               (AccountId c)
    -- ^ The project already exists in the state.
    | ErrHandlerFailed               HandlerError
    -- ^ A contract handler failed and returned an error.
    | ErrInvalidNonce                Nonce
    -- ^ The supplied nonce does not match the account nonce.
    | ErrInvalidFee                  Balance
    -- ^ The transaction fee is not valid.
    | ErrInvalidTx                   Text
    -- ^ The transaction is not valid.
    | ErrOverflow                    (AccountId c)
    -- ^ Executing the transaction would result in an overflow.
    deriving (Generic)

deriving instance (Show (AccountId c), Show (Crypto.Hash c)) => Show (TxError c)
deriving instance (Eq (Signature c), Eq (BlockHash c), Eq (AccountId c)) => Eq (TxError c)
deriving instance (Eq (BlockHash c), Eq (Signature c), Ord (AccountId c)) => Ord (TxError c)

-------------------------------------------------------------------------------

-- | Make a transaction payload with the given messages and author.
mkTxPayload :: [TxMessage c] -> PublicKey c -> TxPayload c
mkTxPayload ms pk = TxPayload
    { txMessages = ms
    , txNonce    = 0
    , txFee      = 0
    , txBurn     = 0
    , txAuthor   = pk
    }

-- | Apply a transaction payload to the world state, and return either an error,
-- or a new world state with the output of the transaction.
applyTxPayload
    :: ( CBOR.Serialise (AccountId c)
       , Ord (AccountId c)
       , Crypto.Hashable c (PublicKey c)
       )
    => TxPayload c
    -- ^ The transaction to apply.
    -> AccountId c
    -- ^ The account of the beneficiary, where fees should be sent.
    -> WorldState c
    -- ^ The input world state.
    -> Either (TxError c) (WorldState c, TxOutput)
applyTxPayload tx@TxPayload{..} beneficiary ws = do
    let author = toAccountId txAuthor

    Account{..} <- lookupAccount author ws

    when (txNonce /= accountNonce) $
        Left (ErrInvalidNonce txNonce)

    when (txFee < minimumTxFee tx) $
        Left (ErrInvalidFee txFee)

    when (txFee + txBurn > accountBalance) $
        Left (ErrInsufficientBalance accountBalance)

    ws' <- transferBalance author beneficiary txFee ws

    -- TODO(cloudhead): Verify transaction size

    applyTxMessages txMessages author
        =<< debitAccount author txBurn
            (adjustAccount incrementNonce author ws')

-- FIXME(adn) This is currently a stub.
verifyTx :: Tx c -> Bool
verifyTx _ = True

-- | Increment the account nonce.
incrementNonce :: Account c -> Account c
incrementNonce acc = acc { accountNonce = accountNonce acc + 1 }

-- | Adjust an account. Does nothing if the account doesn't exist, or the value under the key
-- is not an account.
adjustAccount
    :: CBOR.Serialise (AccountId c)
    => (Account c -> Account c) -> AccountId c -> WorldState c -> WorldState c
adjustAccount f (accountKey -> k) ws =
    case WorldState.lookup k ws of
        Just (AccountVal v) -> WorldState.insert k (AccountVal (f v)) ws
        _                   -> ws

-- | Apply a 'TxMessage' list to a 'WorldState' and return the new state and outputs.
applyTxMessages
    :: (CBOR.Serialise (AccountId c), Ord (AccountId c), Foldable t)
    => t (TxMessage c)
    -> AccountId c
    -> WorldState c
    -> Either (TxError c) (WorldState c, [TxMessageOutput])
applyTxMessages msgs author ws =
    foldM f (ws, []) msgs
  where
    f (s, out) msg = do
        (s', out') <- applyTxMessage msg author s
        pure (s', out <> out')

-- | Apply the transaction message to a world state.
applyTxMessage
    :: ( CBOR.Serialise (AccountId c)
       , Ord (AccountId c)
       )
    => TxMessage c
    -> AccountId c
    -> WorldState c
    -> Either (TxError c) (WorldState c, TxOutput)

applyTxMessage (TxRegisterProject acc) author ws =
    if WorldState.member key ws
       then Left (ErrProjectExists acc)
       else Right ( WorldState.insert key (ProjectVal prj) ws
                  , [] )
  where
    key = accountKey acc
    prj = (mkProject acc) { pMaintainers = Map.singleton author mem }
    mem = mkMember author 0

applyTxMessage (TxUnregisterProject acc) author ws = do
    p@Project{..} <- lookupProject acc    ws
    member        <- lookupAccount author ws

    mapHandlerError (pUnregister p member)
    pure (WorldState.delete projKey ws, [])
  where
    projKey = accountKey acc

applyTxMessage (TxAuthorize acc pk) author ws = do
    p@Project{..} <- lookupProject acc     ws
    member        <- lookupAccount author  ws
    epoch         <- lookupEpoch           ws

    mapHandlerError (pAuthorize pk p member)
    pure (adjust (addKey epoch) projKey ws, [])
  where
    projKey = accountKey acc

    addKey e v = case v of
        ProjectVal p -> ProjectVal $
            p { pMaintainers = Map.insert pk (mkMember pk e) (pMaintainers p) }
        _ -> v

applyTxMessage (TxDeauthorize acc pk) author ws = do
    p@Project{..} <- lookupProject acc     ws
    member        <- lookupAccount author  ws

    mapHandlerError (pDeauthorize pk p member)
    pure (adjust removeKey projKey ws, [])
  where
    projKey = accountKey acc

    removeKey v = case v of
        ProjectVal p -> ProjectVal $
            p { pMaintainers = Map.delete pk (pMaintainers p) }
        _ -> v

applyTxMessage TxCheckpoint{} _ _ws = notImplemented
applyTxMessage TxUpdateContract{} _ _ws = notImplemented

applyTxMessage (TxTransfer _ bal) _ _ | bal == 0 =
    Left (ErrInvalidTransfer bal)
applyTxMessage (TxTransfer receiver _) sender _
    | sender == receiver =
        Left (ErrNotAuthorized sender)
applyTxMessage (TxTransfer receiver bal) sender ws = do
    ws' <- transferBalance sender receiver bal ws

    pure (ws', [])

-- | Add an account to the world state. If the account already exists, it is overwritten. The
-- 'AccountId' of the account is used as key.
insertAccount :: CBOR.Serialise (AccountId c) => Account c -> WorldState c -> WorldState c
insertAccount acc@Account{..} = WorldState.insert (accountKey accountId) (AccountVal acc)

-- | Delete an account from the world state, if it exists.
--
-- Nb. The caller must ensure that the value under the account id is indeed an 'Account'.
deleteAccount :: CBOR.Serialise (AccountId c) => AccountId c -> WorldState c -> WorldState c
deleteAccount id = WorldState.delete (accountKey id)

-- | Transfer balance between two accounts. Uses 'creditAccount' and 'debitAccount'.
transferBalance
    :: (CBOR.Serialise (AccountId c))
    => AccountId c
    -- ^ Transfer sender ("from").
    -> AccountId c
    -- ^ Transfer receiver ("to").
    -> Balance
    -- ^ Transfer amount.
    -> WorldState c
    -- ^ Input state.
    -> Either (TxError c) (WorldState c)
transferBalance a b bal ws =
    debitAccount a bal ws >>= creditAccount b bal

-- | Credit an account with coins.
--
-- Increases the total coin supply and creates the account if it does not exist.
--
-- * Returns 'ErrTypeMismatch' if the value under the given account id is not an account.
-- * Returns 'ErrOverflow' if crediting the account would result in an integer overflow.
--
-- Nb. using this function without a matching 'debitAccount' creates supply.
--
creditAccount
    :: CBOR.Serialise (AccountId c)
    => AccountId c
    -> Balance
    -> WorldState c
    -> Either (TxError c) (WorldState c)
creditAccount id bal ws =
    case lookupAccount id ws of
        Right acc
            | bal > maxBound - accountBalance acc ->
                Left (ErrOverflow id)
            | otherwise ->
                Right $ insertAccount (acc { accountBalance = accountBalance acc + bal }) ws
        Left (ErrKeyNotFound _) -> -- Account doesn't exist, create it.
            Right $ insertAccount ((mkAccount id) { accountBalance = bal }) ws
        Left err ->
            Left err

-- | Debit an account.
--
-- Decreases the total coin supply and deletes the account if its balance reaches zero.
--
-- * Returns 'ErrTypeMismatch' if the value under the given account id is not an account.
-- * Returns 'ErrInsufficientBalance' if the account cannot be debited by the full amount.
-- * Returns 'ErrKeyNotFound' if the account cannot be found.
--
-- Nb. using this function without a matching 'creditAccount' burns supply.
--
debitAccount
    :: CBOR.Serialise (AccountId c)
    => AccountId c
    -> Balance
    -> WorldState c
    -> Either (TxError c) (WorldState c)
debitAccount id bal ws =
    case lookupAccount id ws of
        Right acc
            | bal > accountBalance acc ->
                Left $ ErrInsufficientBalance (accountBalance acc)
            | bal == accountBalance acc ->
                Right $ deleteAccount id ws
            | otherwise ->
                Right $ insertAccount (acc { accountBalance = accountBalance acc - bal }) ws
        Left err ->
            Left err

-- | Map a handler error into a transaction error.
mapHandlerError :: Either HandlerError a -> Either (TxError c) a
mapHandlerError = first ErrHandlerFailed

-- | Lookup a project in the state.
lookupProject
    :: CBOR.Serialise (AccountId c)
    => AccountId c
    -> WorldState c
    -> Either (TxError c) (Project c)
lookupProject acc ws =
    case WorldState.lookup (accountKey acc) ws of
        Just (ProjectVal p) -> Right p
        Nothing             -> Left (ErrKeyNotFound key)
        _                   -> Left (ErrTypeMismatch key)
  where
    key = accountKey acc

-- | Lookup an account in the state.
lookupAccount
    :: CBOR.Serialise (AccountId c)
    => AccountId c
    -> WorldState c
    -> Either (TxError c) (Account c)
lookupAccount acc ws =
    case WorldState.lookup key ws of
        Just (AccountVal a) -> Right a
        Nothing             -> Left (ErrKeyNotFound key)
        _                   -> Left (ErrTypeMismatch key)
  where
    key = accountKey acc

-- | Lookup the current epoch.
lookupEpoch :: WorldState c -> Either (TxError c) Epoch
lookupEpoch ws =
    case WorldState.lookup "epoch" ws of
        Just (NatVal e) -> Right e
        Nothing         -> Left (ErrKeyNotFound "epoch")
        _               -> Left (ErrTypeMismatch "epoch")

-- | The minimum transaction fee that is valid for the given transaction.
minimumTxFee :: TxPayload c -> Balance
minimumTxFee = const 1

-------------------------------------------------------------------------------
-- TxMessage instances
-------------------------------------------------------------------------------

deriving instance
    ( Show (Signature c)
    , Show (BlockHash c)
    , Show (AccountId c)
    , Crypto.HasHashing c
    ) => Show (TxMessage c)

deriving instance
    ( Eq (Signature c)
    , Eq (BlockHash c)
    , Eq (AccountId c)
    ) => Eq (TxMessage c)

deriving instance
    ( Ord (Signature c)
    , Ord (BlockHash c)
    , Ord (AccountId c)
    ) => Ord (TxMessage c)

instance
    ( Serialise (AccountId c)
    , Serialise (Crypto.Hash c)
    , Serialise (Contribution c)
    , Serialise (DependencyUpdate c)
    ) => Serialise (TxMessage c)
  where
    encode (TxRegisterProject acc) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord    0
        <> CBOR.encode        acc
    encode (TxUnregisterProject acc) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord    1
        <> CBOR.encode        acc
    encode (TxAuthorize proj acc) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord    2
        <> CBOR.encode        proj
        <> CBOR.encode        acc
    encode (TxDeauthorize proj acc) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord    3
        <> CBOR.encode        proj
        <> CBOR.encode        acc
    encode (TxCheckpoint proj hsh cs ds) =
           CBOR.encodeListLen 5
        <> CBOR.encodeWord    4
        <> CBOR.encode        proj
        <> CBOR.encode        hsh
        <> CBOR.encode        cs
        <> CBOR.encode        ds
    encode (TxUpdateContract proj) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord    5
        <> CBOR.encode        proj
    encode (TxTransfer acc bal) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord    6
        <> CBOR.encode        acc
        <> CBOR.encode        bal

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLenCanonical CBOR.decodeWordCanonical
        case pre of
            (2, 0) -> TxRegisterProject   <$> CBOR.decode
            (2, 1) -> TxUnregisterProject <$> CBOR.decode
            (3, 2) -> TxAuthorize         <$> CBOR.decode <*> CBOR.decode
            (3, 3) -> TxDeauthorize       <$> CBOR.decode <*> CBOR.decode
            (5, 4) -> TxCheckpoint        <$> CBOR.decode <*> CBOR.decode <*> CBOR.decode <*> CBOR.decode
            (2, 5) -> TxUpdateContract    <$> CBOR.decode
            (3, 6) -> TxTransfer          <$> CBOR.decode <*> CBOR.decode
            e      -> fail $ "Failed decoding TxMessage from CBOR: " ++ show e

-------------------------------------------------------------------------------
-- TxPayload instances
-------------------------------------------------------------------------------

deriving instance
    ( Eq (AccountId c)
    , Eq (Crypto.Hash c)
    , Eq (Signature c)
    , Eq (PublicKey c)
    ) => Eq (TxPayload c)

instance
    ( Serialise (TxMessage c)
    , Serialise (PublicKey c)
    ) => Serialise (TxPayload c)
  where
    encode TxPayload{..} =
           CBOR.encodeListLen 6
        <> CBOR.encodeWord 0
        <> CBOR.encode txMessages
        <> CBOR.encode txNonce
        <> CBOR.encode txFee
        <> CBOR.encode txBurn
        <> CBOR.encode txAuthor
    decode = do
        pre <- liftA2 (,) CBOR.decodeListLenCanonical CBOR.decodeWordCanonical
        case pre of
            (6, 0) ->
                TxPayload
                    <$> CBOR.decode
                    <*> CBOR.decode
                    <*> CBOR.decode
                    <*> CBOR.decode
                    <*> CBOR.decode
            e -> fail $ "Failed decoding TxPayload from CBOR: " ++ show e

-------------------------------------------------------------------------------
-- Tx instances
-------------------------------------------------------------------------------

deriving instance
    ( Show (Signature c)
    , Show (BlockHash c)
    , Show (PublicKey c)
    , Show (TxPayload c)
    , Crypto.HasHashing c
    ) => Show (Tx' version c)

deriving instance
    ( Eq (Signature c)
    , Eq (BlockHash c)
    , Eq (PublicKey c)
    , Eq (TxPayload c)
    ) => Eq (Tx' version c)

deriving instance
    ( Ord (Signature c)
    , Ord (BlockHash c)
    , Ord (PublicKey c)
    , Ord (TxPayload c)
    ) => Ord (Tx' version c)

instance
    ( Serialise (TxMessage c)
    , Serialise (PublicKey c)
    , Serialise (Signature c)
    ) => Serialise (Tx' 1 c)
  where
    encode Tx'{..} =
           CBOR.encodeListLen 4
        <> CBOR.encodeWord16  1
        <> CBOR.encode txPayload
        <> CBOR.encode txNetwork
        <> CBOR.encode txSignature

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLenCanonical CBOR.decodeWord16Canonical
        case pre of
            (4, 1) ->
                Tx' <$> CBOR.decode
                    <*> CBOR.decode
                    <*> CBOR.decode
            e -> fail $ "Failed decoding Tx from CBOR: " ++ show e
