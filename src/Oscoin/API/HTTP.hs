module Oscoin.API.HTTP
    ( run
    , app
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash (toHashed)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.Hash.RealWorld ()
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Crypto.PubKey.RealWorld ()
import           Oscoin.Data.Tx
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Trans as Node
import           Oscoin.Telemetry.Middleware (loggingMiddleware)

import qualified Oscoin.API.HTTP.Handlers as Handlers
import           Oscoin.API.HTTP.Internal

import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai

-- TODO: Don't import this here? Create a HTTP.Routing module?
import qualified Network.Wai as Wai
import           Web.HttpApiData (FromHttpApiData(..))
import           Web.Spock

import           Codec.Serialise (Serialise)
import           Formatting.Buildable (Buildable)

-- | Runner specialised over the production 'Crypto', as we use a separate
-- node handle for tests.
run :: (Serialise s)
    => Int
    -> Node.Handle Crypto (Tx Crypto) s i
    -> IO ()
run port hdl =
    withLogging hdl $ \logging ->
        runApi (api logging) port hdl

app :: ( Typeable c
       , FromHttpApiData (BlockHash c)
       , Serialise s
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , Crypto.HasDigitalSignature c
       , Crypto.HasHashing c
       , Buildable (Crypto.Hash c)
       )
    => Node.Handle c (Tx c) s i
    -> IO Wai.Application
app hdl =
    withLogging hdl $ \logging ->
        spockAsApp $ mkMiddleware (api logging) hdl

-- | Policy for static file serving.
staticFilePolicy :: Wai.Policy
staticFilePolicy =
    Wai.noDots >-> trailingSlashPolicy
               >-> indexPolicy "debug/blockchain"
               >-> Wai.addBase "static"

-- | Entry point for API.
api :: ( Typeable c
       , FromHttpApiData (BlockHash c)
       , Serialise s
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , Crypto.HasDigitalSignature c
       , Crypto.HasHashing c
       , Buildable (Crypto.Hash c)
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

    -- /blockchain/tip -------------------------------------------------------

    get ("blockchain" <//> "tip") Handlers.getTip

    -- /blockchain/best -------------------------------------------------------

    get ("blockchain" <//> "best") Handlers.getBestChain

    -- /transactions ----------------------------------------------------------

    get  "transactions" Handlers.getAllTransactions
    post "transactions" Handlers.submitTransaction

    -- /transactions/:id ------------------------------------------------------

    get ("transactions" <//> var) (Handlers.getTransaction . toHashed)

    -- /state/:chain ----------------------------------------------------------

    get ("state" <//> wildcard) Handlers.getStatePath


-- Internal --------------------------------------------------------------------

withLogging :: Node.Handle c tx s i -> (Wai.Middleware -> IO a) -> IO a
withLogging hdl action =
    action . loggingMiddleware . Node.cfgTelemetry $ Node.hConfig hdl
