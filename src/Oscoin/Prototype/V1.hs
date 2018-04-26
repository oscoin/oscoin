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

newtype Ascii = Ascii ByteString
    deriving (Show, Eq, Ord)

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

deriving instance ToJSON Value

type Fee = Value

data Patch = Patch
    { patchAuthors   :: Set PublicKey
    , patchTimestamp :: UTCTime
    , patchHash      :: Hashed Patch
    , patchMessage   :: Text
    , patchChangeset :: Text
    } deriving (Show, Eq)

-- Chain ----------------------------------------------------------------------

data Chain tx m s = Chain
    { fold :: Fold (Tx tx) m s }

genesisChain :: MonadFork m => Chain GenesisTx m GenesisState
genesisChain = Chain
    { fold = genesisFold }

accountChain :: MonadFork m => Chain AccountTx m AccountState
accountChain = Chain
    { fold = accountFold }

repoChain :: MonadFork m => Chain RepoTx m RepoState
repoChain = Chain
    { fold = repoFold }

-- FOLDS ----------------------------------------------------------------------

type Fold tx m s = [tx] -> s -> m s

class Monad m => MonadFork m where
    fork :: Fold tx m s -> s -> m ()

genesisFold :: MonadFork m => Fold (Tx GenesisTx) m GenesisState
genesisFold (Tx{txPayload = tx}:txs) s =
    case tx of
        OpenAccount id addr pk -> do
            fork accountFold (accountState id addr)
            genesisFold txs s { genesisAccounts = Map.insert id pk (genesisAccounts s) }
        Bond from to val ->
            let bond       = Bonded from to val
                f Nothing  = Just (Set.singleton bond)
                f (Just x) = Just (Set.insert    bond x)
             in genesisFold txs s { genesisBonds = Map.alter f to (genesisBonds s) }
genesisFold [] s = pure s

accountFold :: MonadFork m => Fold (Tx AccountTx) m AccountState
accountFold _txs s = pure s

repoFold :: MonadFork m => Fold (Tx RepoTx) m RepoState
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
      Record Patch                     -- ^ Record a new given patch.
    | Pick   [Key] (Hashed Patch)      -- ^ Pick a patch from another repository, given its path and hash.
    | Branch [Key] (Hashed Patch) Key  -- ^ Branch from another repository@ref.

-- States ---------------------------------------------------------------------

data Bonded = Bonded Address Address Value
    deriving (Show, Eq, Ord)

data GenesisState = GenesisState
    { genesisAccounts :: Map AccountId PublicKey
    , genesisBonds    :: Map Address (Set Bonded)
    } deriving (Show)

data AccountState = AccountState
    { accountId    :: AccountId
    , accountAddr  :: Address
    , accountRepos :: Set RepoId
    , accountKeys  :: Set PublicKey
    } deriving (Show)

accountState :: AccountId -> Address -> AccountState
accountState id addr = AccountState id addr mempty mempty

-- TODO: Should `accountRepos` be a `Map RepoId (Set PublicKey)`?

data RepoState = RepoState
    { repoId      :: RepoId
    , repoPatches :: Set Patch
    , repoKeys    :: Set PublicKey
    } deriving (Show)
