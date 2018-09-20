module Oscoin.API.HTTP
    ( RadTx
    , run
    , api
    , withAPI
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Types (RadTx)
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad
import           Oscoin.Environment

import qualified Oscoin.API.HTTP.Handlers as Handlers
import           Oscoin.API.HTTP.Internal

-- TODO: Don't import this here? Create a HTTP.Routing module?
import           Web.Spock (get, middleware, post, root, var, wildcard, (<//>))

withAPI :: Environment -> (Api Rad.Env i () -> m a) -> m a
withAPI env f = f (api env)

-- | Entry point for API.
api :: Environment -> Api Rad.Env i ()
api env = do
    middleware $ loggingMiddleware env

    -- / ----------------------------------------------------------------------

    get root Handlers.root

    -- /blocks/:id ------------------------------------------------------------

    get ("blocks" <//> var) Handlers.getBlock

    -- /blockchain/best -------------------------------------------------------

    get ("blockchain" <//> "best") Handlers.getBestChain

    -- /transactions ----------------------------------------------------------

    get  "transactions" Handlers.getAllTransactions
    post "transactions" Handlers.submitTransaction

    -- /transactions/:id ------------------------------------------------------

    get ("transactions" <//> var) Handlers.getTransaction

    -- /state/:key ------------------------------------------------------------

    get ("state" <//> wildcard) Handlers.getStatePath
