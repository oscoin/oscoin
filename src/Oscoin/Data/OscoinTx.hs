{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | The Transaction.
--
module Oscoin.Data.OscoinTx where

import           Oscoin.Prelude hiding (length)

import           Oscoin.Configuration (Network(..))
import           Oscoin.Crypto.Address
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey
import           Oscoin.Data.Ledger

import qualified Codec.Serialise as CBOR
import qualified Crypto.Data.Auth.Tree as WorldState
import qualified Data.Aeson as JSON
import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Tx
-------------------------------------------------------------------------------

-- | A transaction on the ledger with the default version and flags.
type Tx c = Tx' 1 0 c

-- | A transaction with parametric version and flags.
data Tx' (version :: Nat) (flags :: Nat) c = Tx'
    { txPayload :: TxPayload c
    -- ^ Transaction message.
    , txNetwork :: Network
    -- ^ Network this transaction belongs to.
    , txNonce   :: Word64
    -- ^ Account nonce.
    , txFee     :: Balance
    -- ^ Transaction fee to miner.
    , txBurn    :: Balance
    -- ^ Transaction burn.
    } deriving (Generic)

-- | Transaction version.
txVersion :: forall version flags c. KnownNat version => Tx' version flags c -> Word16
txVersion _ = fromIntegral (natVal (Proxy @version))

-- | Transaction flags.
txFlags :: forall version flags c. KnownNat flags => Tx' version flags c -> Word16
txFlags _ = fromIntegral (natVal (Proxy @flags))

-- | A transaction payload.
data TxPayload c =
      TxRegisterProject   (Address c)
    -- ^ Register a project.
    | TxUnregisterProject (Address c)
    -- ^ Unregister a project.
    | TxAuthorize         (Address c) (Address c)
    -- ^ Authorize an account to operate on a project.
    | TxDeauthorize       (Address c) (Address c)
    -- ^ De-authorize an account to operate on a project.
    | TxCheckpoint        (Address c) (Crypto.Hash c) [Contribution c] [DependencyUpdate c]
    -- ^ Checkpoint a project's state.
    | TxUpdateContract    (Address c)
    -- ^ Update a project's contract.
    | TxTransfer          (Address c) (Address c) Balance
    -- ^ Transfer balance between two accounts.
    deriving (Generic)

-- | Transaction output, included in the receipts tree. (Placeholder)
data TxOutput = TxOutput
    deriving (Show, Eq, Generic)

instance CBOR.Serialise TxOutput

instance JSON.ToJSON   TxOutput
instance JSON.FromJSON TxOutput

instance ByteArrayAccess TxOutput where
    length = length . LBS.toStrict . CBOR.serialise
    withByteArray ba = withByteArray (LBS.toStrict . CBOR.serialise $ ba)

-- | A transaction evaluation error.
data TxError c =
      ErrInsufficientBalance         Balance
    -- ^ Insufficient balance in the account to execute transfer.
    | ErrNotAuthorized               (Address c)
    -- ^ This account is not authorized to execute the operation.
    | ErrInvalidTransfer             Balance
    -- ^ A balance transfer of this amount is not valid.
    | ErrKeyNotFound                 StateKey
    -- ^ The key was not found in the state.
    | ErrTypeMismatch                StateKey
    -- ^ The value under this key does not have the expected type.
    | ErrProjectExists               (Address c)
    -- ^ The project already exists in the state.
    | ErrHandlerFailed               HandlerError
    -- ^ A contract handler failed and returned an error.
    | ErrInvalidNonce                Nonce
    -- ^ The supplied nonce does not match the account nonce.
    | ErrInvalidFee                  Balance
    -- ^ The transaction fee is not valid.
    | ErrInvalidTx                   Text
    -- ^ The transaction is not valid.
    | ErrOverflow                    (Address c)
    -- ^ Executing the transaction would result in an overflow.
    deriving (Generic)

deriving instance (Show (PublicKey c), Show (Crypto.Hash c)) => Show (TxError c)
deriving instance (Eq (Signature c), Eq (BlockHash c), Eq (PublicKey c)) => Eq (TxError c)
deriving instance (Eq (BlockHash c), Eq (Signature c), Ord (PublicKey c)) => Ord (TxError c)

-------------------------------------------------------------------------------

-- | Make a transaction with the given payload.
mkTx :: TxPayload c -> Tx c
mkTx p = Tx'
    { txPayload = p
    , txNetwork = Devnet
    , txNonce   = 0
    , txFee     = 0
    , txBurn    = 0
    }

-- | Apply a transaction to the world state, and return either an error,
-- or a new world state with the output of the transaction.
applyTx
    :: ( CBOR.Serialise (PublicKey c)
       , Ord (PublicKey c)
       )
    => Tx c
    -- ^ The transaction to apply.
    -> Address c
    -- ^ The author or \"signer\" of the transaction.
    -> WorldState c
    -- ^ The input world state.
    -> Either (TxError c) (WorldState c, TxOutput)
applyTx tx@Tx'{..} author ws = do
    Account{..} <- lookupAccount author ws

    when (txNonce /= accountNonce) $
        Left (ErrInvalidNonce txNonce)

    when (txFee < minimumTxFee tx) $
        Left (ErrInvalidFee txFee)

    when (txFee + txBurn > accountBalance) $
        Left (ErrInsufficientBalance accountBalance)

    -- TODO(cloudhead): Transfer fee
    -- TODO(cloudhead): Destroy burn
    -- TODO(cloudhead): Verify transaction size
    -- TODO(cloudhead): Increment nonce

    applyTxPayload txPayload author ws

-- FIXME(adn) This is currently a stub.
verifyTx :: Tx c -> Bool
verifyTx _ = True

-- | Apply the transaction payload to a world state.
applyTxPayload
    :: ( CBOR.Serialise (PublicKey c)
       , Ord (PublicKey c)
       )
    => TxPayload c
    -> Address c
    -> WorldState c
    -> Either (TxError c) (WorldState c, TxOutput)

applyTxPayload (TxRegisterProject addr) _ ws =
    if WorldState.member key ws
       then Left (ErrProjectExists addr)
       else Right ( WorldState.insert key (ProjectVal (mkProject addr)) ws
                  , TxOutput )
  where
    key = addressKey addr

applyTxPayload (TxUnregisterProject addr) author ws = do
    p@Project{..} <- lookupProject addr   ws
    member        <- lookupMember  author ws

    mapHandlerError (pUnregister p member)
    pure (WorldState.delete projKey ws, TxOutput)
  where
    projKey = addressKey addr

applyTxPayload (TxAuthorize addr pk) author ws = do
    p@Project{..} <- lookupProject addr    ws
    member        <- lookupMember  author  ws
    epoch         <- lookupEpoch           ws

    mapHandlerError (pAuthorize pk p member)
    pure (adjust (addKey epoch) projKey ws, TxOutput)
  where
    projKey = addressKey addr

    addKey e v = case v of
        ProjectVal p -> ProjectVal $
            p { pMaintainers = Map.insert pk (mkMember pk e) (pMaintainers p) }
        _ -> v

applyTxPayload (TxDeauthorize addr pk) author ws = do
    p@Project{..} <- lookupProject addr    ws
    member        <- lookupMember  author  ws

    mapHandlerError (pDeauthorize pk p member)
    pure (adjust removeKey projKey ws, TxOutput)
  where
    projKey = addressKey addr

    removeKey v = case v of
        ProjectVal p -> ProjectVal $
            p { pMaintainers = Map.delete pk (pMaintainers p) }
        _ -> v

applyTxPayload TxCheckpoint{} _ _ws = notImplemented
applyTxPayload TxUpdateContract{} _ _ws = notImplemented

applyTxPayload (TxTransfer _ _ bal) _ _ | bal == 0 =
    Left (ErrInvalidTransfer bal)
applyTxPayload (TxTransfer sender receiver bal) author ws = do
    when (author /= sender) $
        Left (ErrNotAuthorized author)

    ws'  <- debitAccount  sender   bal ws
        >>= creditAccount receiver bal

    pure (ws', TxOutput)

-- | Credit an account with coins.
--
-- Increases the total coin supply and creates the account if it does not exist.
--
-- * Returns 'ErrTypeMismatch' if the value under the given address is not an account.
-- * Returns 'ErrOverflow' if crediting the account would result in an integer overflow.
--
-- Nb. using this function without a matching 'debitAccount' creates supply.
--
creditAccount
    :: CBOR.Serialise (PublicKey c)
    => Address c
    -> Balance
    -> WorldState c
    -> Either (TxError c) (WorldState c)
creditAccount addr bal ws =
    case lookupAccount addr ws of
        Right acc
            | bal > maxBound - accountBalance acc ->
                Left (ErrOverflow addr)
            | otherwise ->
                Right $ WorldState.insert (addressKey addr)
                    (AccountVal $ acc { accountBalance = accountBalance acc + bal }) ws
        Left (ErrKeyNotFound key) -> -- Account doesn't exist, create it.
            Right $ WorldState.insert key
                (AccountVal $ (mkAccount addr) { accountBalance = bal }) ws
        Left err ->
            Left err

-- | Debit an account.
--
-- Decreases the total coin supply and deletes the account if its balance reaches zero.
--
-- * Returns 'ErrTypeMismatch' if the value under the given address is not an account.
-- * Returns 'ErrInsufficientBalance' if the account cannot be debited by the full amount.
-- * Returns 'ErrKeyNotFound' if the account cannot be found.
--
-- Nb. using this function without a matching 'creditAccount' burns supply.
--
debitAccount
    :: CBOR.Serialise (PublicKey c)
    => Address c
    -> Balance
    -> WorldState c
    -> Either (TxError c) (WorldState c)
debitAccount addr bal ws =
    case lookupAccount addr ws of
        Right acc
            | bal > accountBalance acc ->
                Left $ ErrInsufficientBalance (accountBalance acc)
            | bal == accountBalance acc ->
                Right $ WorldState.delete key ws
            | otherwise ->
                Right $ WorldState.insert key
                    (AccountVal $ acc { accountBalance = accountBalance acc - bal }) ws
        Left err ->
            Left err
  where
    key = addressKey addr

-- | Map a handler error into a transaction error.
mapHandlerError :: Either HandlerError a -> Either (TxError c) a
mapHandlerError = first ErrHandlerFailed

-- | Lookup a member in the state.
lookupMember
    :: CBOR.Serialise (PublicKey c)
    => Address c
    -> WorldState c
    -> Either (TxError c) (Member c)
lookupMember addr ws =
    case WorldState.lookup key ws of
        Just (MemberVal m) -> Right m
        Nothing            -> Left (ErrKeyNotFound key)
        _                  -> Left (ErrTypeMismatch key)
  where
    key = addressKey addr

-- | Lookup a project in the state.
lookupProject
    :: CBOR.Serialise (PublicKey c)
    => Address c
    -> WorldState c
    -> Either (TxError c) (Project c)
lookupProject addr ws =
    case WorldState.lookup (addressKey addr) ws of
        Just (ProjectVal p) -> Right p
        Nothing             -> Left (ErrKeyNotFound key)
        _                   -> Left (ErrTypeMismatch key)
  where
    key = addressKey addr

-- | Lookup an account in the state.
lookupAccount
    :: CBOR.Serialise (PublicKey c)
    => Address c
    -> WorldState c
    -> Either (TxError c) (Account c)
lookupAccount addr ws =
    case WorldState.lookup key ws of
        Just (AccountVal a) -> Right a
        Nothing             -> Left (ErrKeyNotFound key)
        _                   -> Left (ErrTypeMismatch key)
  where
    key = addressKey addr

-- | Lookup the current epoch.
lookupEpoch :: WorldState c -> Either (TxError c) Epoch
lookupEpoch ws =
    case WorldState.lookup "epoch" ws of
        Just (NatVal e) -> Right e
        Nothing         -> Left (ErrKeyNotFound "epoch")
        _               -> Left (ErrTypeMismatch "epoch")

-- | The minimum transaction fee that is valid for the given transaction.
minimumTxFee :: Tx c -> Balance
minimumTxFee = const 1

-------------------------------------------------------------------------------
-- TxPayload instances
-------------------------------------------------------------------------------

deriving instance
    ( Show (Signature c)
    , Show (BlockHash c)
    , Show (PublicKey c)
    , Crypto.HasHashing c
    ) => Show (TxPayload c)

deriving instance
    ( Eq (Signature c)
    , Eq (BlockHash c)
    , Eq (PublicKey c)
    ) => Eq (TxPayload c)

deriving instance
    ( Ord (Signature c)
    , Ord (BlockHash c)
    , Ord (PublicKey c)
    ) => Ord (TxPayload c)

-------------------------------------------------------------------------------
-- Tx instances
-------------------------------------------------------------------------------

deriving instance
    ( Show (Signature c)
    , Show (BlockHash c)
    , Show (PublicKey c)
    , Crypto.HasHashing c
    ) => Show (Tx' version flags c)

deriving instance
    ( Eq (Signature c)
    , Eq (BlockHash c)
    , Eq (PublicKey c)
    ) => Eq (Tx' version flags c)

deriving instance
    ( Ord (Signature c)
    , Ord (BlockHash c)
    , Ord (PublicKey c)
    ) => Ord (Tx' version flags c)
