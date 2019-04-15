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
                 , maxBound
                 , mod
                 , ($)
                 , (.)
                 , (<>)
                 )

import           Oscoin.Crypto.Address (Address)
import           Oscoin.Crypto.Address.Serialisation (serializeAddress)
import           Oscoin.Crypto.Blockchain (Height)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey (Signature, Signed)

import qualified Codec.Serialise as CBOR
import           Crypto.Data.Auth.Tree (Tree)
import qualified Crypto.Data.Auth.Tree as WorldState
import           Data.ByteArray (ByteArrayAccess)
import           Data.ByteString (ByteString)
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
    | MemberVal  (Member c)
    | NatVal     Natural

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

-------------------------------------------------------------------------------
-- Core types
-------------------------------------------------------------------------------

-- | A balance of oscoin in the smallest denomination.
type Balance = Word64

-- | An account nonce to prevent replay attacks.
type Nonce = Word64

-- | An account which holds a balance.
data Account c = Account
    { accountAddr    :: Address c
    -- ^ The account identifier.
    , accountBalance :: Balance
    -- ^ The oscoin balance.
    , accountNonce   :: Nonce
    -- ^ The nonce is equal to the number of transactions
    -- made from this account.
    }

mkAccount :: Address c -> Account c
mkAccount addr = Account
    { accountAddr    = addr
    , accountBalance = 0
    , accountNonce   = 0
    }

-- | Convert an 'Address' into a StateKey
addressKey
    :: ByteArrayAccess (Crypto.ShortHash c)
    => CBOR.Serialise (Crypto.ShortHash c)
    => Address c -> StateKey
addressKey = serializeAddress

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

instance Eq (Checkpoint c) where
    a == b = checkpointNumber a == checkpointNumber b

instance Ord (Checkpoint c) where
    a <= b = checkpointNumber a <= checkpointNumber b

-- | A contribution to a project.
data Contribution c = Contribution
    { contribHash       :: Crypto.Hash c
    -- ^ The hash of the off-chain contribution artefact.
    , contribParentHash :: Maybe (Crypto.Hash c)
    -- ^ The parent contribution, or 'Nothing' if it's the first.
    -- Matches with 'contribHash', forming a hash-linked list.
    , contribAddr       :: Address c
    -- ^ The address of the contributor.
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
    , Crypto.HasHashing c
    ) => Show (Contribution c)

deriving instance
    ( Eq (Signature c)
    , Eq (BlockHash c)
    ) => Eq (Contribution c)

deriving instance
    ( Ord (Signature c)
    , Ord (BlockHash c)
    ) => Ord (Contribution c)

-------------------------------------------------------------------------------

-- | An update to a project dependency.
data DependencyUpdate c =
      Depend       (Address c) (Crypto.Hash c)
    -- ^ Start depending on a project starting from the given state hash.
    | Undepend     (Address c)
    -- ^ Stop depending on a project.
    deriving (Generic)

deriving instance (Eq (Crypto.Hash c))   => Eq   (DependencyUpdate c)
deriving instance (Ord (Crypto.Hash c))  => Ord  (DependencyUpdate c)
deriving instance (Show (Crypto.Hash c)) => Show (DependencyUpdate c)

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

    , pMaintainers     :: Map (Address c) (Member c)
    , pContributors    :: Map (Address c) (Member c)
    , pSupporters      :: Map (Address c) (Member c)
    , pDependencies    :: Map (Address c) (Dependency c)

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

mkProject :: Address c -> Project c
mkProject addr = Project
    { pAccount          = mkAccount addr
    , pMaintainers      = mempty
    , pContributors     = mempty
    , pSupporters       = mempty
    , pDependencies     = mempty
    , pCheckpoints      = mempty
    , pSendTransfer     = defaultSendTransfer
    , pReceiveTransfer  = defaultReceiveTransfer
    , pReceiveReward    = defaultReceiveReward
    , pCheckpoint       = defaultCheckpoint
    , pUnregister       = defaultUnregister
    , pAuthorize        = defaultAuthorize
    , pDeauthorize      = defaultDeauthorize
    , pUpdateContract   = defaultUpdateContract
    }

-- | Get the address of a project.
projectAddr :: Project c -> Address c
projectAddr = accountAddr . pAccount

-- | A delegation of oscoin between two accounts. Allows members to become
-- /supporters/ by delegating their voting rights to a project. Allows projects
-- to use the delegated tokens to vote.
data Delegation c = Delegation
    { delegDelegator  :: Address c
    -- ^ Account delegating.
    , delegReceiver   :: Address c
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
    { memberAccount       :: Address c
    , memberDelegation    :: Maybe (Delegation c)
    , memberSince         :: Epoch
    , memberContributions :: Set (Contribution c)
    }

mkMember
    :: Address c -> Epoch -> Member c
mkMember addr e = Member
    { memberAccount       = addr
    , memberDelegation    = Nothing
    , memberSince         = e
    , memberContributions = Set.empty
    }

-- | A dependency between two projects.
data Dependency c = Dependency
    { depFrom :: Address c
    , depTo   :: Address c
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
       Member c                 -- ^ Transaction signer
    -> Address c                -- ^ Sender
    -> Address c                -- ^ Receiver
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
    -> Address c               -- ^ Sender
    -> Project c
    -> [(Address c, Balance)]

defaultReceiveTransfer :: ReceiveTransfer' c
defaultReceiveTransfer = depositToFund

mkReceiveTransfer :: ReceiveTransfer' c
mkReceiveTransfer _ _ _ = []

depositToFund :: ReceiveTransfer' c
depositToFund bal _ p =
    [(projectAddr p, bal)]

-------------------------------------------------------------------------------
-- ReceiveReward
-------------------------------------------------------------------------------

type ReceiveReward' c =
       Balance                    -- ^ The balance being rewarded
    -> Epoch                      -- ^ The current epoch
    -> Project c                  -- ^ The project dictionary
    -> [(Address c, Balance)]     -- ^ A balance distribution

-- | By default, burn the reward.
defaultReceiveReward :: ReceiveReward' c
defaultReceiveReward = burnReward

-- | Burn the reward!
burnReward :: ReceiveReward' c
burnReward _ _ _ = []

-- | Distribute reward equally to all project members. Store any remainder in the project fund.
distributeRewardEqually :: ReceiveReward' c
distributeRewardEqually bal _epoch p@Project{..} =
    let members     = Map.keysSet $ pContributors <> pMaintainers <> pSupporters
        (dist, rem) = distribute bal members
     in (projectAddr p, rem) : dist

-------------------------------------------------------------------------------
-- Checkpoint
-------------------------------------------------------------------------------

type Checkpoint' c =
       Checkpoint c               -- ^ Checkpoint data
    -> Project c                  -- ^ Project data
    -> Member  c                  -- ^ Transaction signer
    -> Either HandlerError ()

-- | By default, authorize any checkpoint signed by a maintainer.
defaultCheckpoint :: Checkpoint' c
defaultCheckpoint _ = requireMaintainer

-------------------------------------------------------------------------------
-- Unregister
-------------------------------------------------------------------------------

type Unregister' c =
       Project c                  -- ^ Project data
    -> Member c                   -- ^ Transaction signer
    -> Either HandlerError ()

defaultUnregister :: Unregister' c
defaultUnregister = requireMaintainer

-------------------------------------------------------------------------------
-- Authorize
-------------------------------------------------------------------------------

type Authorize' c =
       Address c                    -- ^ Key to be added
    -> Project c                    -- ^ Project data
    -> Member c                     -- ^ Transaction signer
    -> Either HandlerError ()

defaultAuthorize :: Authorize' c
defaultAuthorize _ = requireMaintainer

-------------------------------------------------------------------------------
-- Deauthorize
-------------------------------------------------------------------------------

type Deauthorize' c =
       Address c                  -- ^ Key to be removed
    -> Project c                  -- ^ Project data
    -> Member c                   -- ^ Transaction signer
    -> Either HandlerError ()

defaultDeauthorize :: Deauthorize' c
defaultDeauthorize _ = requireMaintainer

-------------------------------------------------------------------------------
-- UpdateContract
-------------------------------------------------------------------------------

type UpdateContract' c =
       Handler                  -- ^ Handler to be updated
    -> HandlerParams            -- ^ New handler parameters
    -> Project c                -- ^ Project data
    -> Member c                 -- ^ Transaction signer
    -> Either HandlerError ()

defaultUpdateContract :: UpdateContract' c
defaultUpdateContract _ _ = requireMaintainer

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | Distribute a balance equally to a set of address, returning the remainder.
distribute :: Balance -> Set (Address c) -> ([(Address c, Balance)], Balance)
distribute bal accs =
    ([(acc, share) | acc <- Set.toList accs], remainder)
  where
    share     = bal `div` fromIntegral (Set.size accs)
    remainder = bal `mod` share

-- | Return 'Right ()' if the member is a project maintainer and 'Left' otherwise.
requireMaintainer :: Project c -> Member c -> Either HandlerError ()
requireMaintainer Project{..} Member{..} =
    if Map.member memberAccount pMaintainers
       then Right ()
       else Left  (HandlerError "Signer must be a project maintainer")
