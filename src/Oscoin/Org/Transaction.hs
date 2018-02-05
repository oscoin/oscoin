module Oscoin.Org.Transaction
    ( Tx
    , validateTx
    , setTx
    ) where

import           Oscoin.Prelude
import           Oscoin.Org
import           Oscoin.Address
import           Oscoin.Crypto.PubKey

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
    deriving (Generic)

instance Binary Voice

data Tx =
      AccountTx     AccountId Address Coin PublicKey [Permission]
    | PatchTx       RepoId BranchId PatchId [Patch]
    | VoiceIssueTx  IssueId Voice
    | AmmendIssueTx IssueId Text Text [PatchId]
    | SendTx        Address Address Coin
    | SetTx         OrgId OrgKey OrgVal
    deriving (Generic)

instance Binary Tx

validateTx :: Signed Tx -> Either Error (Signed Tx)
validateTx stx@(Signed tx _) = do
    validateTx' tx
    pure stx

validateTx' :: Tx -> Either Error Tx
validateTx' tx@(SetTx orgId orgKey _)
  | not (T.null orgId)
  , not (T.null orgKey) = Right tx
  | otherwise = Left "Invalid org id or key"
validateTx' _ =
    notImplemented

setTx :: OrgId -> OrgKey -> OrgVal -> Tx
setTx = SetTx

