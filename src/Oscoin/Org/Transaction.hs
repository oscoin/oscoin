module Oscoin.Org.Transaction
    ( Tx
    , validateTransaction
    , applyTransaction
    , submitTransaction
    , verifySignature
    , setTx
    ) where

import           Oscoin.Prelude
import           Oscoin.Org
import           Oscoin.Address
import           Oscoin.Crypto.PubKey (PublicKey, Signed(..))
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Node.State as State
import qualified Oscoin.Storage.Transaction as Mempool

import           Data.Binary
import qualified Data.Text as T

type Coin = ()
type Patch = ()
type Permission = ()

type IssueId = ()
type AccountId = ()
type BranchId = ()
type RepoId = ()
type PatchId = ()

data Voice = Yea | Nay
    deriving (Show, Eq, Ord, Generic)

instance Binary Voice

data Tx =
      AccountTx     AccountId Address Coin [Permission]
    | PatchTx       RepoId BranchId PatchId [Patch]
    | VoiceIssueTx  IssueId Voice
    | AmmendIssueTx IssueId Text Text [PatchId]
    | SendTx        Address Address Coin
    | SetTx         OrgId OrgKey OrgVal
    deriving (Show, Eq, Ord, Generic)

instance Binary Tx

validateTransaction :: Signed Tx -> Either Error (Signed Tx)
validateTransaction stx@(Signed tx _) = do
    validateTransaction' tx
    pure stx

validateTransaction' :: Tx -> Either Error Tx
validateTransaction' tx@(SetTx orgId orgKey _)
  | not (T.null orgId)
  , not (T.null orgKey) = Right tx
  | otherwise = Left "Invalid org id or key"
validateTransaction' _ =
    notImplemented

setTx :: OrgId -> OrgKey -> OrgVal -> Tx
setTx = SetTx

verifySignature :: PublicKey -> Signed Tx -> Either Error Tx
verifySignature pubKey stx =
    if Crypto.verify pubKey stx
       then Right (sigMessage stx)
       else Left  "Invalid signature"

applyTransaction :: Tx -> OrgTree -> OrgTree
applyTransaction (SetTx org key val) tree =
    setPath org ["data", key] val tree
applyTransaction _ _ =
    notImplemented

submitTransaction :: Tx -> State.StorageT Tx IO ()
submitTransaction tx =
    Mempool.updateMempool (Mempool.addTx tx)
