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
import qualified Oscoin.Node.Trans as Node.Trans
import           Oscoin.Telemetry (TelemetryStore)
import           Oscoin.Telemetry.Middleware (telemetryMiddleware)

import qualified Oscoin.API.HTTP.Handlers as Handlers
import           Oscoin.API.HTTP.Internal

import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai

-- TODO: Don't import this here? Create a HTTP.Routing module?
import qualified Network.Wai as Wai
import           Web.Spock

import           Codec.Serialise (Serialise)
import           Data.Aeson (ToJSON)

run :: (ToJSON s, Serialise s) => Int -> Environment -> Node.Handle RadTx Rad.Env s i -> IO ()
run port env hdl =
    let telemetryStore = Node.cfgTelemetryStore . Node.Trans.hConfig $ hdl
    in runApi (api env telemetryStore) port hdl

app :: (ToJSON s, Serialise s) => Environment -> Node.Handle RadTx Rad.Env s i -> IO Wai.Application
app env hdl =
    let telemetryStore = Node.cfgTelemetryStore . Node.Trans.hConfig $ hdl
    in spockAsApp $ mkMiddleware (api env telemetryStore) hdl

-- | Policy for static file serving.
staticFilePolicy :: Wai.Policy
staticFilePolicy =
    Wai.noDots >-> trailingSlashPolicy
               >-> indexPolicy "debug/blockchain"
               >-> Wai.addBase "static"

-- | Entry point for API.
api :: (ToJSON s, Serialise s) => Environment -> TelemetryStore -> Api s i ()
api env telemetryStore = do
    middleware $ loggingMiddleware env
               . telemetryMiddleware telemetryStore
               . Wai.staticPolicy staticFilePolicy

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

    get ("transactions" <//> var) (Handlers.getTransaction . toHashed)

    -- /state/:chain ----------------------------------------------------------

    get ("state" <//> wildcard) Handlers.getStatePath
