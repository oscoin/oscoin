module Oscoin.Org.Transaction where

import Oscoin.Prelude
import Oscoin.Org
import Oscoin.Address
import Oscoin.Crypto.PubKey

type Coin = ()
type Patch = ()
type Permission = ()

type IssueId = ()
type AccountId = ()
type BranchId = ()
type RepoId = ()
type PatchId = ()

data Voice = Yea | Nay

data Tx =
      AccountTx     AccountId Address Coin PublicKey [Permission]
    | PatchTx       RepoId BranchId PatchId [Patch]
    | VoiceIssueTx  IssueId Voice
    | AmmendIssueTx IssueId Text Text [PatchId]
    | SendTx        Address Address Coin
    | SetTx         OrgId OrgKey OrgVal
