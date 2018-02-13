module Oscoin.HTTP.API where

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.Org.Transaction as Org
import           Oscoin.Crypto.PubKey (Signed)

import qualified Oscoin.HTTP.Handlers as Handlers
import           Oscoin.HTTP.Internal

import           Data.Aeson ((.=), object)
import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai

-- TODO: Don't import this here? Create a HTTP.Routing module?
import           Web.Spock (middleware, get, post, root, json, (<//>), var)

-- | Entry point for API.
api :: Environment -> Api (Signed Org.Tx) ()
api env = do
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
