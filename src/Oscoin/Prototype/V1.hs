module Oscoin.Prototype.V1 where

import           Oscoin.Prelude

import           Oscoin.Address
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.PubKey

import           Data.Time.Clock (UTCTime)
import           Data.Aeson (ToJSON, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.Extended as Base64
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import           Data.Binary
import           Control.Monad.State

incoming :: [Tx a]
incoming = []

-------------------------------------------------------------------------------

-- | The fold function.
type Fold tx m c = [tx] -> c -> m c

-- | The class of Monads that can fork branches.
class (Monad m, Fork m ~ m) => MonadFork m where
    type Fork m :: * -> *

    -- | Create a new branch, given a path and initial branch state.
    fork :: [Key] -> Branch (Fork m) -> m ()

-- | A tree of branches, accessed by path.
newtype BlockTree m = BlockTree
    { fromBlockTree :: Map [Key] (Branch m) }

-- | The genesis state.
genesis :: (Binary (Tx AccountTx), MonadFork m) => BlockTree m
genesis = BlockTree . Map.fromList $
    [([], Branch (GenesisChain mempty mempty))]

-- | A branch. (Note the use of existential type.)
data Branch m =
      forall c tx. HasFold c tx m => Branch c

-- | A branch that can be folded onto itself, given a list of @tx@.
class MonadFork m => HasFold c tx m | c -> tx where
    fold :: Fold tx m c
    fromTx :: Binary tx => c -> Tx LBS.ByteString -> m (Tx tx)

instance (Binary (Tx AccountTx), MonadFork m) => HasFold GenesisChain (Tx GenesisTx) m where
    fold = genesisFold
    fromTx _ tx@Tx{txPayload} =
        pure $ tx {txPayload = Binary.decode txPayload}

instance MonadFork m => HasFold AccountChain (Tx AccountTx) m where
    fold = accountFold
    fromTx _ tx@Tx{txPayload} =
        pure $ tx {txPayload = Binary.decode txPayload}

instance MonadFork m => HasFold RepoChain (Tx RepoTx) m where
    fold = repoFold
    fromTx _ tx@Tx{txPayload} =
        pure $ tx {txPayload = Binary.decode txPayload}

newtype ForkT m a = ForkT (StateT (BlockTree m) m a)
    deriving (Functor, Applicative, Monad, MonadState (BlockTree m))

instance (Monad m, Fork (ForkT m) ~ ForkT m) => MonadFork (ForkT m) where
    type Fork (ForkT m) = m

    fork :: [Key] -> Branch (Fork (ForkT m)) -> ForkT m ()
    fork path branch =
        modify $ \(BlockTree cs) ->
            BlockTree (Map.insert path branch cs)

newtype Ascii = Ascii ByteString
    deriving (Show, Eq, Ord, Generic)

instance Binary Ascii

instance ToJSON Ascii where
    toJSON (Ascii bs) = toJSON (decodeUtf8 bs)

type Name = Text
type AccountId = Key
type RepoId = Key
type Nonce = Word64

-- | Key, part of a path, eg. "acme".
type Key = Ascii

-- | Token value.
newtype Value = Value Word64
    deriving (Show, Num, Eq, Ord, Generic)

instance Binary Value

deriving instance ToJSON Value

type Fee = Value

data Patch = Patch
    { patchAuthors   :: Set PublicKey
    , patchTimestamp :: UTCTime
    , patchHash      :: Hashed Patch
    , patchMessage   :: Text
    , patchChangeset :: Text
    } deriving (Show, Eq, Ord)

-- FOLDS ----------------------------------------------------------------------

genesisFold :: (MonadFork m, Binary (Tx AccountTx)) => Fold (Tx GenesisTx) m GenesisChain
genesisFold (Tx{txPayload = tx}:txs) s =
    case tx of
        OpenAccount id addr pk -> do
            fork [id] (Branch $ accountChain id addr)
            genesisFold txs s { genesisAccounts = Map.insert id pk (genesisAccounts s) }
        Bond from to val ->
            let bond       = Bonded from to val
                f Nothing  = Just (Set.singleton bond)
                f (Just x) = Just (Set.insert    bond x)
             in genesisFold txs s { genesisBonds = Map.alter f to (genesisBonds s) }
genesisFold [] s = pure s

accountFold :: MonadFork m => Fold (Tx AccountTx) m AccountChain
accountFold (Tx{txPayload = tx}:txs) s =
    case tx of
        OpenRepository id -> do
            fork [accountId s, id] (Branch $ repoChain id (accountKeys s))
            accountFold txs s { accountRepos = Set.insert id (accountRepos s) }
        CloseRepository id ->
            accountFold txs s { accountRepos = Set.delete id (accountRepos s) }
        AccountKeys pks ->
            accountFold txs s { accountKeys = Set.union (Set.fromList pks) (accountKeys s) }
accountFold [] s = pure s

repoFold :: MonadFork m => Fold (Tx RepoTx) m RepoChain
repoFold _txs s = pure s

-- TXs ------------------------------------------------------------------------

data Tx a = Tx
    { txPayload :: a
    , txPath    :: [Key]
    , txNonce   :: Nonce
    , txFee     :: Fee
    } deriving (Eq, Ord, Functor)

-- POST /
data GenesisTx =
      OpenAccount AccountId Address PublicKey
    | Bond        Address Address Value
    deriving (Generic)

instance Binary GenesisTx

instance ToJSON GenesisTx where
    toJSON (OpenAccount key addr pk) =
        Aeson.object [ "tx"   .= ("open-account" :: Text)
                     , "id"   .= key
                     , "addr" .= addr
                     , "pk"   .= toJSON (Base64.encodeBinary pk)
                     ]
    toJSON (Bond from to val) =
        Aeson.object [ "tx"     .= ("bond" :: Text)
                     , "from"   .= from
                     , "to"     .= to
                     , "value"  .= val
                     ]

-- TODO
-- Whether OpenRepository is allowed depends on how much tokens are bonded
-- behind the organization? This is our rate-limitting. We don't need it at
-- every level, just at the top level. That way we only have bonding in the
-- genesis chain.

-- POST /:account/
data AccountTx =
      OpenRepository  RepoId
    | CloseRepository RepoId
    | AccountKeys     [PublicKey]

-- POST /:account/:repo/
data RepoTx =
      Record  Patch                     -- ^ Record a new given patch.
    | Pick    [Key] (Hashed Patch)      -- ^ Pick a patch from another repository, given its path and hash.
    | Branch' [Key] (Hashed Patch) Key  -- ^ Branch from another repository@ref.

-- States ---------------------------------------------------------------------

data Bonded = Bonded Address Address Value
    deriving (Show, Eq, Ord)

data GenesisChain = GenesisChain
    { genesisAccounts :: Map AccountId PublicKey
    , genesisBonds    :: Map Address (Set Bonded)
    } deriving (Show)

data AccountChain = AccountChain
    { accountId    :: AccountId
    , accountAddr  :: Address
    , accountRepos :: Set RepoId
    , accountKeys  :: Set PublicKey
    } deriving (Show)

accountChain :: AccountId -> Address -> AccountChain
accountChain id addr = AccountChain id addr mempty mempty

-- TODO: Should `accountRepos` be a `Map RepoId (Set PublicKey)`?

data RepoChain = RepoChain
    { repoId      :: RepoId
    , repoPatches :: Set Patch
    , repoKeys    :: Set PublicKey
    } deriving (Show)

repoChain :: RepoId -> Set PublicKey -> RepoChain
repoChain id keys =
    RepoChain id mempty keys
