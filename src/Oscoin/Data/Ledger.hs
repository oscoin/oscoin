{-# LANGUAGE UndecidableInstances #-}

-- | Core ledger data types.
--
module Oscoin.Data.Ledger where

-- Since the handler functions implemented in this module may
-- eventually be turned into smart contracts, we keep track of
-- all imports and try to keep them to a minimum.
import           Oscoin.Prelude
                 ( Either(..)
                 , Eq(..)
                 , Generic
                 , Int
                 , IsString
                 , Map
                 , Maybe(..)
                 , Monoid(..)
                 , Ord(..)
                 , Set
                 , Show
                 , Text
                 , Word64
                 , Word8
                 , div
                 , fromIntegral
                 , map
                 , maxBound
                 , mod
                 , sum
                 , ($)
                 , (.)
                 , (<>)
                 )

import           Oscoin.Crypto.Blockchain (Height)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey (PublicKey, Signature, Signed)

import qualified Codec.Serialise as CBOR
import           Crypto.Data.Auth.Tree (Tree)
import qualified Crypto.Data.Auth.Tree as WorldState
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Numeric.Natural

-------------------------------------------------------------------------------
-- World State
-------------------------------------------------------------------------------

-- | The state of the world, materialized from transactions.
type WorldState c = Tree StateKey (StateVal c)

-- | State lookup key.
type StateKey = ByteString

-- | State value.
data StateVal c =
      AccountVal (Account c)
    | ProjectVal (Project c)
    | NatVal     Natural

deriving instance (Eq (PublicKey c)) => Eq (StateVal c)

-- | Like 'Map.adjust', but for 'WorldState'.
adjust :: (StateVal c -> StateVal c) -> StateKey -> WorldState c -> WorldState c
adjust f k ws =
    case WorldState.lookup k ws of
        Just v  -> WorldState.insert k (f v) ws
        Nothing -> ws

-- | Like 'Map.alter', but for 'WorldState'.
alter
    :: (Maybe (StateVal c) -> Maybe (StateVal c))
    -> StateKey
    -> WorldState c
    -> WorldState c
alter f k ws =
    case f (WorldState.lookup k ws) of
        Just v  -> WorldState.insert k v ws
        Nothing -> WorldState.delete k ws

-- | /O(n)/. Returns the total supply of tokens in the ledger.
balanceTotal :: WorldState c -> Balance
balanceTotal ws = sum $ map f (WorldState.toList ws)
  where
    f (_, AccountVal acc) = accountBalance acc
    f _                   = 0

-------------------------------------------------------------------------------
-- Core types
-------------------------------------------------------------------------------

-- | A balance of oscoin in the smallest denomination.
type Balance = Word64

-- | An account nonce to prevent replay attacks.
type Nonce = Word64

-- | An account identifier.
type AccountId c = PublicKey c

-- | An account which holds a balance.
data Account c = Account
    { accountId      :: AccountId c
    -- ^ The account identifier.
    , accountBalance :: Balance
    -- ^ The oscoin balance.
    , accountNonce   :: Nonce
    -- ^ The nonce is equal to the number of transactions
    -- made from this account.
    }

deriving instance (Eq (PublicKey c)) => Eq (Account c)

mkAccount :: AccountId c -> Account c
mkAccount acc = Account
    { accountId      = acc
    , accountBalance = 0
    , accountNonce   = 0
    }

-- | Convert an 'AccountId' into a StateKey
accountKey
    :: CBOR.Serialise (PublicKey c)
    => AccountId c
    -> StateKey
accountKey = LBS.toStrict . CBOR.serialise

-------------------------------------------------------------------------------

-- | A number of blocks representing a long period of time.
type Epoch = Natural

-- | A number of epochs.
type Epochs = Natural

-- | A contribution signed-off by a maintainer.
type Signoff c = Signed c (Contribution c)

-------------------------------------------------------------------------------
-- Checkpoints
-------------------------------------------------------------------------------

-- | A project checkpoint.
data Checkpoint c = Checkpoint
    { checkpointNumber        :: Natural
    -- ^ The checkpoint number, starting from zero.
    , checkpointStateHash     :: Crypto.Hash c
    -- ^ The hash of the project state at this checkpoint.
    , checkpointContributions :: [Contribution c]
    -- ^ The new contributions since the last checkpoint.
    -- Contributions /must/ be hash-linked to maintain the integrity of the list.
    -- See 'Contribution' for details.
    , checkpointDependencies  :: [DependencyUpdate c]
    -- ^ Updates to the dependencies since the last checkpoint.
    }

deriving instance
    (Eq (PublicKey c), Eq (Crypto.Hash c), Eq (Signature c)) => Eq (Checkpoint c)

instance
    (Eq (PublicKey c), Eq (Crypto.Hash c), Eq (Signature c)) => Ord (Checkpoint c)
  where
    a <= b = checkpointNumber a <= checkpointNumber b

-- | A contribution to a project.
data Contribution c = Contribution
    { contribHash       :: Crypto.Hash c
    -- ^ The hash of the off-chain contribution artefact.
    , contribParentHash :: Maybe (Crypto.Hash c)
    -- ^ The parent contribution, or 'Nothing' if it's the first.
    -- Matches with 'contribHash', forming a hash-linked list.
    , contribAccount    :: AccountId c
    -- ^ The account id of the contributor.
    , contribSignoff    :: Maybe (Signoff c)
    -- ^ An optional sign-off signature.
    , contribLabels     :: Set Label
    -- ^ A set of labels used to categorize the contribution.
    } deriving (Generic)

-- | A label used to tag a contribution.
newtype Label = Label Word8
    deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

deriving instance
    ( Show (Signature c)
    , Show (BlockHash c)
    , Show (PublicKey c)
    , Crypto.HasHashing c
    ) => Show (Contribution c)

deriving instance
    ( Eq (Signature c)
    , Eq (BlockHash c)
    , Eq (PublicKey c)
    ) => Eq (Contribution c)

deriving instance
    ( Ord (Signature c)
    , Ord (BlockHash c)
    , Ord (PublicKey c)
    ) => Ord (Contribution c)

-------------------------------------------------------------------------------

-- | An update to a project dependency.
data DependencyUpdate c =
      Depend       (AccountId c) (Crypto.Hash c)
    -- ^ Start depending on a project starting from the given state hash.
    | Undepend     (AccountId c)
    -- ^ Stop depending on a project.
    deriving (Generic)

deriving instance (Eq (Crypto.Hash c), Eq (PublicKey c))     => Eq   (DependencyUpdate c)
deriving instance (Ord (Crypto.Hash c), Ord (PublicKey c))   => Ord  (DependencyUpdate c)
deriving instance (Show (Crypto.Hash c), Show (PublicKey c)) => Show (DependencyUpdate c)

-- | Additional signatures used to authorize a transaction in a
-- /multi-sig/ scenario.
newtype Signatures c = Signatures [Signed () c]

-------------------------------------------------------------------------------
-- Project
-------------------------------------------------------------------------------

-- | A project.
data Project c = Project
    { pAccount         :: Account c
    -- ^ The project account or /fund/.

    , pMaintainers     :: Map (AccountId c) (Member c)
    , pContributors    :: Map (AccountId c) (Member c)
    , pSupporters      :: Map (AccountId c) (Member c)
    , pDependencies    :: Map (AccountId c) (Dependency c)

    , pCheckpoints     :: Set (Checkpoint c)

    -- * Contract
    , pSendTransfer    :: SendTransfer' c
    , pReceiveTransfer :: ReceiveTransfer' c
    , pReceiveReward   :: ReceiveReward' c
    , pCheckpoint      :: Checkpoint' c
    , pUnregister      :: Unregister' c
    , pAuthorize       :: Authorize' c
    , pDeauthorize     :: Deauthorize' c
    , pUpdateContract  :: UpdateContract' c
    }

instance (Eq (PublicKey c)) => Eq (Project c) where
    (==) a b = projectId a == projectId b

mkProject :: Ord (PublicKey c) => AccountId c -> Project c
mkProject acc = Project
    { pAccount          = mkAccount acc
    , pMaintainers      = mempty
    , pContributors     = mempty
    , pSupporters       = mempty
    , pDependencies     = mempty
    , pCheckpoints      = Set.empty
    , pSendTransfer     = defaultSendTransfer
    , pReceiveTransfer  = defaultReceiveTransfer
    , pReceiveReward    = defaultReceiveReward
    , pCheckpoint       = defaultCheckpoint
    , pUnregister       = defaultUnregister
    , pAuthorize        = defaultAuthorize
    , pDeauthorize      = defaultDeauthorize
    , pUpdateContract   = defaultUpdateContract
    }

-- | Get the account id of a project.
projectId :: Project c -> AccountId c
projectId = accountId . pAccount

-- | A delegation of oscoin between two accounts. Allows members to become
-- /supporters/ by delegating their voting rights to a project. Allows projects
-- to use the delegated tokens to vote.
data Delegation c = Delegation
    { delegDelegator  :: AccountId c
    -- ^ Account delegating.
    , delegReceiver   :: AccountId c
    -- ^ Account receiving the delegation.
    , delegBalance    :: Balance
    -- ^ Balance being delegated.
    , delegCommitment :: Height
    -- ^ Time commitment of the delegation.
    , delegSince      :: Height
    -- ^ Starting time of the delegation.
    }

-- | A member of a project. Includes maintainers, contributors and supporters.
data Member c = Member
    { memberAccount       :: AccountId c
    , memberDelegation    :: Maybe (Delegation c)
    , memberSince         :: Epoch
    , memberContributions :: Set (Contribution c)
    }

instance (Eq (PublicKey c)) => Eq (Member c) where
    (==) a b = memberAccount a == memberAccount b

mkMember
    :: AccountId c -> Epoch -> Member c
mkMember acc e = Member
    { memberAccount       = acc
    , memberDelegation    = Nothing
    , memberSince         = e
    , memberContributions = Set.empty
    }

-- | A dependency between two projects.
data Dependency c = Dependency
    { depFrom :: AccountId c
    , depTo   :: AccountId c
    , depHash :: Crypto.Hash c
    -- ^ Hash of the checkpoint state being depended on.
    } deriving (Generic)

-- | Identifies a contract handler.
data Handler =
      SendTransferHandler
    | ReceiveRewardHandler
    | ReceiveTransferHandler
    | CheckpointHandler
    | AuthorizeHandler
    | DeauthorizeHandler
    | UnregisterHandler
    | UpdateContractHandler
    deriving (Show, Eq, Ord)

-- | Dictionary used to parametrize a handler.
type HandlerParams = Map ParamKey ParamVal

-- | Parameter key.
type ParamKey = Text

-- | Parameter value.
data ParamVal =
      Integer  Int
    -- ^ Expresses positive or negative whole numbers.
    | Rational Int Int
    -- ^ Expresses ratios, eg. 1/2.
    deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

newtype HandlerError = HandlerError Text
    deriving (Show, Eq, Ord, IsString)

-------------------------------------------------------------------------------
-- SendTransfer
-------------------------------------------------------------------------------

type SendTransfer' c =
       Account c                -- ^ Transaction signer
    -> AccountId c              -- ^ Sender
    -> AccountId c              -- ^ Receiver
    -> Balance                  -- ^ Balance to transfer
    -> Signatures c             -- ^ Sign-offs
    -> Either HandlerError ()

defaultSendTransfer :: SendTransfer' c
defaultSendTransfer = mkSendTransfer maxBound

mkSendTransfer :: Balance -> SendTransfer' c
mkSendTransfer maxBalance = sendTransfer
  where
    sendTransfer _signer _from _to bal _sigs =
        if bal <= maxBalance
           then Right ()
           else Left "Max balance exceeded for transfer"

-------------------------------------------------------------------------------
-- ReceiveTransfer
-------------------------------------------------------------------------------

type ReceiveTransfer' c =
       Balance
    -> AccountId c               -- ^ Sender
    -> Project c
    -> [(AccountId c, Balance)]

defaultReceiveTransfer :: ReceiveTransfer' c
defaultReceiveTransfer = depositToFund

mkReceiveTransfer :: ReceiveTransfer' c
mkReceiveTransfer _ _ _ = []

depositToFund :: ReceiveTransfer' c
depositToFund bal _ p =
    [(projectId p, bal)]

-------------------------------------------------------------------------------
-- ReceiveReward
-------------------------------------------------------------------------------

type ReceiveReward' c =
       Balance                    -- ^ The balance being rewarded
    -> Epoch                      -- ^ The current epoch
    -> Project c                  -- ^ The project dictionary
    -> [(AccountId c, Balance)]   -- ^ A balance distribution

-- | By default, burn the reward.
defaultReceiveReward :: ReceiveReward' c
defaultReceiveReward = burnReward

-- | Burn the reward!
burnReward :: ReceiveReward' c
burnReward _ _ _ = []

-- | Distribute reward equally to all project members. Store any remainder in the project fund.
distributeRewardEqually :: Ord (PublicKey c) => ReceiveReward' c
distributeRewardEqually bal _epoch p@Project{..} =
    let members     = Map.keysSet $ pContributors <> pMaintainers <> pSupporters
        (dist, rem) = distribute bal members
     in (projectId p, rem) : dist

-------------------------------------------------------------------------------
-- Checkpoint
-------------------------------------------------------------------------------

type Checkpoint' c =
       Checkpoint c               -- ^ Checkpoint data
    -> Project c                  -- ^ Project data
    -> Account  c                 -- ^ Transaction signer
    -> Either HandlerError ()

-- | By default, authorize any checkpoint signed by a maintainer.
defaultCheckpoint :: Ord (PublicKey c) => Checkpoint' c
defaultCheckpoint _ = requireMaintainer

-------------------------------------------------------------------------------
-- Unregister
-------------------------------------------------------------------------------

type Unregister' c =
       Project c                  -- ^ Project data
    -> Account c                  -- ^ Transaction signer
    -> Either HandlerError ()

defaultUnregister :: Ord (PublicKey c) => Unregister' c
defaultUnregister = requireMaintainer

-------------------------------------------------------------------------------
-- Authorize
-------------------------------------------------------------------------------

type Authorize' c =
       AccountId c                  -- ^ Key to be added
    -> Project c                    -- ^ Project data
    -> Account c                    -- ^ Transaction signer
    -> Either HandlerError ()

defaultAuthorize :: Ord (PublicKey c) => Authorize' c
defaultAuthorize _ = requireMaintainer

-------------------------------------------------------------------------------
-- Deauthorize
-------------------------------------------------------------------------------

type Deauthorize' c =
       AccountId c                -- ^ Key to be removed
    -> Project c                  -- ^ Project data
    -> Account c                  -- ^ Transaction signer
    -> Either HandlerError ()

defaultDeauthorize :: Ord (PublicKey c) => Deauthorize' c
defaultDeauthorize _ = requireMaintainer

-------------------------------------------------------------------------------
-- UpdateContract
-------------------------------------------------------------------------------

type UpdateContract' c =
       Handler                  -- ^ Handler to be updated
    -> HandlerParams            -- ^ New handler parameters
    -> Project c                -- ^ Project data
    -> Account c                -- ^ Transaction signer
    -> Either HandlerError ()

defaultUpdateContract :: Ord (PublicKey c) => UpdateContract' c
defaultUpdateContract _ _ = requireMaintainer

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | Distribute a balance equally to a set of accounts, returning the remainder.
distribute :: Balance -> Set (AccountId c) -> ([(AccountId c, Balance)], Balance)
distribute bal accs =
    ([(acc, share) | acc <- Set.toList accs], remainder)
  where
    share     = bal `div` fromIntegral (Set.size accs)
    remainder = bal `mod` share

-- | Return 'Right ()' if the account is a project maintainer and 'Left' otherwise.
requireMaintainer
    :: Ord (PublicKey c)
    => Project c
    -> Account c
    -> Either HandlerError ()
requireMaintainer Project{..} Account{..} =
    if Map.member accountId pMaintainers
       then Right ()
       else Left  (HandlerError "Signer must be a project maintainer")
