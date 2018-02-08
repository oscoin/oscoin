{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.HTTP where

import           Oscoin.Prelude
import           Oscoin.Environment
import           Oscoin.Org (Org, OrgId)
import qualified Oscoin.Org.Transaction as Org
import qualified Oscoin.Crypto.PubKey as Crypto

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

-- TODO: Move this to Internal module.
run :: [(OrgId, Org)] -> Environment -> Int -> IO ()
run orgs env port =
    runSpock port (mkMiddleware orgs env)

mkMiddleware :: [(OrgId, Org)] -> Environment -> IO Wai.Middleware
mkMiddleware orgs env = do
    spockCfg <- defaultSpockCfg () (PCConn connBuilder) state
    spock spockCfg (app env)
  where
    conn        = State.connect ()
    connBuilder = ConnBuilder conn State.close (PoolCfg 1 1 30)
    state       = mkState { stOrgs = orgs }

loggingMiddleware :: Environment -> Wai.Middleware
loggingMiddleware Production = Wai.logStdout
loggingMiddleware Development = Wai.logStdoutDev
loggingMiddleware Testing = id

-- | Entry point for the Spock application.
app :: Environment -> Api (Crypto.Signed Org.Tx) ()
app env = do
    middleware $ loggingMiddleware env
               . Wai.staticPolicy (Wai.noDots >-> Wai.addBase ".")

    -- / ----------------------------------------------------------------------

    get root $ do
        json $ object [ "ok" .= True ]

    -- /node/mempool ----------------------------------------------------------

    get  ("node" <//> "mempool") Handlers.getAllTransactions
    post ("node" <//> "mempool") Handlers.submitTransaction

    -- /node/mempool/:id ------------------------------------------------------

    get ("node" <//> "mempool" <//> var) Handlers.getTransaction

    -- /blocks/:id ------------------------------------------------------

    -- /orgs ------------------------------------------------------------------

    get  "orgs" Handlers.getOrgs

    -- /orgs/:org -------------------------------------------------------------

    get ("orgs" <//> var) Handlers.getOrg

    -- /orgs/:org/repos -------------------------------------------------------

    get ("orgs" <//> var <//> "repos") Handlers.getRepos

    -- /orgs/:org/repos/:repo -------------------------------------------------

    get ("orgs" <//> var <//> "repos" <//> var) Handlers.getRepo

    -- /orgs/:org/repos/:repo/patches -----------------------------------------

    -- /orgs/:org/data/:key ---------------------------------------------------

    get ("orgs" <//> var <//> "data" <//> var) Handlers.getOrgKey

    -- /orgs/:org/members/:member ---------------------------------------------

    get ("orgs" <//> var <//> "members" <//> var) Handlers.getMember
