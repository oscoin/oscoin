module Oscoin.Org where

import           Oscoin.Prelude
import           Oscoin.Storage.State (Key)

type OrgId = Text
type OrgKey = Text
type OrgVal = ByteString

type MemberId = Text

mkOrgKey :: OrgId -> OrgKey -> Key
mkOrgKey org key =
    org <> "/" <> key
