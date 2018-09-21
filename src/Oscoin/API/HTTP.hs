module Oscoin.API.HTTP
    ( run
    , app
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Types (RadTx)
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad
import           Oscoin.Crypto.Hash (toHashed)
import           Oscoin.Environment
import qualified Oscoin.Node as Node

import qualified Oscoin.API.HTTP.Handlers as Handlers
import           Oscoin.API.HTTP.Internal

import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai

-- TODO: Don't import this here? Create a HTTP.Routing module?
import qualified Network.Wai as Wai
import           Web.Spock

run :: Int -> Environment -> Node.Handle RadTx Rad.Env i -> IO ()
run port env hdl = runApi (api env) port hdl

app :: Environment -> Node.Handle RadTx Rad.Env i -> IO Wai.Application
app env hdl = spockAsApp $ mkMiddleware (api env) hdl

-- | Entry point for API.
api :: Environment -> Api Rad.Env i ()
api env = do
    middleware $ loggingMiddleware env
               . Wai.staticPolicy (Wai.noDots >-> Wai.addBase ".")

    -- / ----------------------------------------------------------------------

    get root Handlers.root

    -- /blocks/:id ------------------------------------------------------------

    get ("blocks" <//> var) (Handlers.getBlock . toHashed)

    -- /blockchain/best -------------------------------------------------------

    get ("blockchain" <//> "best") Handlers.getBestChain

    -- /transactions ----------------------------------------------------------

    get  "transactions" Handlers.getAllTransactions
    post "transactions" Handlers.submitTransaction

    -- /transactions/:id ------------------------------------------------------

    get ("transactions" <//> var) (Handlers.getTransaction . toHashed)

    -- /state/:chain ----------------------------------------------------------

    get ("state" <//> wildcard) Handlers.getStatePath
