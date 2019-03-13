module Oscoin.API.HTTP
    ( run
    , app
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Types (RadTx)
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Blockchain.Block (BlockHash, Sealed)
import           Oscoin.Crypto.Hash (toHashed)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.Hash.RealWorld ()
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Crypto.PubKey.RealWorld ()
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
import           Web.HttpApiData (FromHttpApiData(..))
import           Web.Spock

import           Codec.Serialise (Serialise)
import           Data.Aeson (FromJSON, ToJSON)
import           Formatting.Buildable (Buildable)

withTelemetryMiddleware :: Node.Handle c tx e s i
                        -> (Wai.Middleware -> IO a)
                        -> IO a
withTelemetryMiddleware hdl action = do
    let handle = Node.cfgTelemetry . Node.Trans.hConfig $ hdl
    action (telemetryMiddleware handle)

-- | Runner specialised over the production 'Crypto', as we use a separate
-- node handle for tests.
run
    :: ( Serialise s
       , ToJSON s
       )
    => Int
    -> Node.Handle Crypto (RadTx Crypto) (Rad.Env Crypto) s i
    -> IO ()
run port hdl =
    withTelemetryMiddleware hdl $ \mdlware -> runApi (api mdlware) port hdl

app
    :: ( ToJSON (Sealed c s)
       , ToJSON (Crypto.PublicKey c)
       , Typeable c
       , FromHttpApiData (BlockHash c)
       , Serialise s
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , Crypto.HasDigitalSignature c
       , Crypto.HasHashing c
       , FromJSON (BlockHash c)
       , FromJSON (Crypto.PublicKey c)
       , FromJSON (Crypto.Signature c)
       , Ord (Crypto.Hash c)
       , Buildable (Crypto.Hash c)
       , ToJSON (Crypto.Hash c)
       , ToJSON (Crypto.Signature c)
       )
    => Node.Handle c (RadTx c) (Rad.Env c) s i
    -> IO Wai.Application
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
api
    :: ( ToJSON (Sealed c s)
       , ToJSON (Crypto.PublicKey c)
       , Typeable c
       , FromHttpApiData (BlockHash c)
       , Serialise s
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , Crypto.HasDigitalSignature c
       , Crypto.HasHashing c
       , FromJSON (BlockHash c)
       , FromJSON (Crypto.PublicKey c)
       , FromJSON (Crypto.Signature c)
       , Ord (Crypto.Hash c)
       , Buildable (Crypto.Hash c)
       , ToJSON (Crypto.Hash c)
       , ToJSON (Crypto.Signature c)
       )
    => Wai.Middleware
    -> Api c s i ()
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
