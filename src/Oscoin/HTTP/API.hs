module Oscoin.HTTP.API
    ( ApiTx
    , api
    , withAPI
    ) where

import           Oscoin.Prelude

import           Oscoin.Environment

import qualified Oscoin.HTTP.Handlers as Handlers
import           Oscoin.HTTP.Internal
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad

import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai

-- TODO: Don't import this here? Create a HTTP.Routing module?
import           Web.Spock (get, middleware, post, root, var, (<//>), wildcard)

withAPI :: Environment -> (Api Rad.Env i () -> m a) -> m a
withAPI env f = f (api env)

-- | Entry point for API.
api :: Environment -> Api Rad.Env i ()
api env = do
    middleware $ loggingMiddleware env
               . Wai.staticPolicy (Wai.noDots >-> Wai.addBase ".")

    -- / ----------------------------------------------------------------------

    get root Handlers.root

    -- /transactions ----------------------------------------------------------

    get  "transactions" Handlers.getAllTransactions
    post "transactions" Handlers.submitTransaction

    -- /transactions/:id ------------------------------------------------------

    get ("transactions" <//> var) Handlers.getTransaction

    -- /state/:key -------------------------------------------------------

    get ("state" <//> wildcard) Handlers.getStatePath

