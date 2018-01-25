module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude
import qualified Oscoin.Node.State as State
import           Oscoin.HTTP.Internal (ApiAction, getBody)
import           Oscoin.Org (OrgId, OrgKey, OrgVal)

getOrgKey :: OrgId -> OrgKey -> ApiAction ()
getOrgKey _org _key = notImplemented

setOrgKey :: OrgId -> OrgKey -> ApiAction ()
setOrgKey org key = do
    val :: OrgVal <- getBody
    io $ State.setOrgKey org key val
