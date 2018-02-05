module Oscoin.Org.Transaction where

import Oscoin.Prelude
import Oscoin.Org
import Oscoin.Address
import Oscoin.Crypto.PubKey

import Data.Binary

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

setTx :: OrgId -> OrgKey -> OrgVal -> Tx
setTx = SetTx

validateTx :: Tx -> Either Error Tx
validateTx = Right
