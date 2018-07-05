module Oscoin.HTTP.API where

import           Oscoin.Prelude

import qualified Oscoin.Account.Transaction as Account
import           Oscoin.Crypto.PubKey (Signed)
import           Oscoin.Environment

import qualified Oscoin.HTTP.Handlers as Handlers
import           Oscoin.HTTP.Internal

import           Data.Aeson (object, (.=))
import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai

-- TODO: Don't import this here? Create a HTTP.Routing module?
import           Web.Spock (get, json, middleware, post, root, var, (<//>))

-- | Entry point for API.
api :: Environment -> Api (Signed Account.Tx) i ()
api env = do
    middleware $ loggingMiddleware env
               . Wai.staticPolicy (Wai.noDots >-> Wai.addBase ".")

    -- / ----------------------------------------------------------------------

    get root $
        json $ object [ "ok" .= True ]

    -- /node/mempool ----------------------------------------------------------

    get  ("node" <//> "mempool") Handlers.getAllTransactions
    post ("node" <//> "mempool") Handlers.submitTransaction

    -- /node/mempool/:id ------------------------------------------------------

    get ("node" <//> "mempool" <//> var) Handlers.getTransaction

    -- /blocks/:id ------------------------------------------------------------

    -- /accounts --------------------------------------------------------------

    get  "accounts" Handlers.getAccounts

    -- /accounts/:account -----------------------------------------------------

    get ("accounts" <//> var) Handlers.getAccount

    -- /accounts/:account/repos -----------------------------------------------

    get ("accounts" <//> var <//> "repos") Handlers.getRepos

    -- /accounts/:account/repos/:repo -----------------------------------------

    get ("accounts" <//> var <//> "repos" <//> var) Handlers.getRepo

    -- /accounts/:account/repos/:repo/patches ---------------------------------

    -- /accounts/:account/data/:key -------------------------------------------

    get ("accounts" <//> var <//> "data" <//> var) Handlers.getAccountDataKey

    -- /accounts/:account/members/:member -------------------------------------

    get ("accounts" <//> var <//> "members" <//> var) Handlers.getMember
