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

    -- /node/mempool ----------------------------------------------------------

    get  ("node" <//> "mempool") Handlers.getAllTransactions
    post ("node" <//> "mempool") Handlers.submitTransaction

    -- /node/mempool/:id ------------------------------------------------------

    get ("node" <//> "mempool" <//> var) Handlers.getTransaction

    -- /node/state/:key -------------------------------------------------------

    get ("node" <//> "state" <//> wildcard) Handlers.getStatePath
