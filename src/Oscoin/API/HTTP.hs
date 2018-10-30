module Oscoin.API.HTTP
    ( run
    , app
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Types (RadTx)
import           Oscoin.Crypto.Hash (toHashed)
import qualified Oscoin.Data.RadicleTx as Rad
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

-- | Policy for static file serving.
staticFilePolicy :: Wai.Policy
staticFilePolicy =
    Wai.noDots >-> trailingSlashPolicy
               >-> indexPolicy "debug/blockchain"
               >-> Wai.addBase "static"

-- | Entry point for API.
api :: Environment -> Api i ()
api env = do
    middleware $ loggingMiddleware env
               . Wai.staticPolicy staticFilePolicy

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
