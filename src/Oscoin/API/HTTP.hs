module Oscoin.API.HTTP
    ( run
    , app
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Types (RadTx)
import           Oscoin.Crypto.Hash (toHashed)
import qualified Oscoin.Data.RadicleTx as Rad
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Trans as Node.Trans
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

withTelemetryMiddleware :: Node.Handle tx e s i
                        -> (Wai.Middleware -> IO a)
                        -> IO a
withTelemetryMiddleware hdl action = do
    let telemetryStore = Node.cfgTelemetryStore . Node.Trans.hConfig $ hdl
    action (telemetryMiddleware telemetryStore)

run :: (ToJSON s, Serialise s) => Int -> Node.Handle RadTx Rad.Env s i -> IO ()
run port hdl =
    withTelemetryMiddleware hdl $ \mdlware -> runApi (api mdlware) port hdl

app :: (ToJSON s, Serialise s) => Node.Handle RadTx Rad.Env s i -> IO Wai.Application
app hdl =
    withTelemetryMiddleware hdl $ \mdlware ->
        spockAsApp $ mkMiddleware (api mdlware) hdl

-- | Policy for static file serving.
staticFilePolicy :: Wai.Policy
staticFilePolicy =
    Wai.noDots >-> trailingSlashPolicy
               >-> indexPolicy "debug/blockchain"
               >-> Wai.addBase "static"

-- | Entry point for API.
api :: (ToJSON s, Serialise s) => Wai.Middleware -> Api s i ()
api mdlware = do
    middleware $ mdlware
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
