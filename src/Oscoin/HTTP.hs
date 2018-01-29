{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.HTTP where

import           Oscoin.Prelude
import           Oscoin.Environment

import qualified Oscoin.HTTP.Handlers as Handlers
import           Oscoin.HTTP.Internal
import qualified Oscoin.Node.State as State

import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson ((.=), object)
import qualified Network.Wai as Wai
import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

run :: Environment -> Int -> IO ()
run env port =
    runSpock port (mkMiddleware env)

mkMiddleware :: Environment -> IO Wai.Middleware
mkMiddleware env = do
    spockCfg <- defaultSpockCfg () (PCConn connBuilder) (State ())
    spock spockCfg (app env)
  where
    conn        = State.connect ()
    connBuilder = ConnBuilder conn State.close (PoolCfg 1 1 30)

loggingMiddleware :: Environment -> Wai.Middleware
loggingMiddleware Production = Wai.logStdout
loggingMiddleware Development = Wai.logStdoutDev
loggingMiddleware Testing = id

-- | Entry point for the Spock application.
app :: Environment -> Api ()
app env = do
    middleware $ loggingMiddleware env
               . Wai.staticPolicy (Wai.noDots >-> Wai.addBase ".")

    -- / ----------------------------------------------------------------------

    get root $ do
        json $ object [ "ok" .= True ]

    -- /orgs ------------------------------------------------------------------

    get  "orgs" Handlers.getOrgs

    -- /orgs/:org -------------------------------------------------------------

    get    ("orgs" <//> var) Handlers.getOrg
    post   ("orgs" <//> var) Handlers.createOrg
    put    ("orgs" <//> var) Handlers.updateOrg
    delete ("orgs" <//> var) Handlers.deleteOrg

    -- /orgs/:org/repos -------------------------------------------------------

    get ("orgs" <//> var <//> "repos") Handlers.getRepos

    -- /orgs/:org/repos/:repo -------------------------------------------------

    get    ("orgs" <//> var <//> "repos" <//> var) Handlers.getRepo
    post   ("orgs" <//> var <//> "repos" <//> var) Handlers.createRepo
    put    ("orgs" <//> var <//> "repos" <//> var) Handlers.updateRepo
    delete ("orgs" <//> var <//> "repos" <//> var) Handlers.deleteRepo

    -- /orgs/:org/repos/:repo/patches -----------------------------------------

    post   ("orgs" <//> var <//> "repos" <//> var <//> "patches") Handlers.submitPatch

    -- /orgs/:org/data/:key ---------------------------------------------------

    get    ("orgs" <//> var <//> "data" <//> var) Handlers.getOrgKey
    put    ("orgs" <//> var <//> "data" <//> var) Handlers.setOrgKey
    delete ("orgs" <//> var <//> "data" <//> var) Handlers.deleteOrgKey

    -- /orgs/:org/members/:member ---------------------------------------------

    get    ("orgs" <//> var <//> "members" <//> var) Handlers.getMember
    post   ("orgs" <//> var <//> "members" <//> var) Handlers.createMember
    put    ("orgs" <//> var <//> "members" <//> var) Handlers.updateMember
    delete ("orgs" <//> var <//> "members" <//> var) Handlers.deleteMember
