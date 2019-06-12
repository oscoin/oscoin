module Oscoin.API.HTTP
    ( run
    , app
    , api
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Blockchain.Block (BlockHash, SealedBlock)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.Hash.RealWorld ()
import           Oscoin.Crypto.PubKey.RealWorld ()
import qualified Oscoin.Data.Ledger as Ledger
import           Oscoin.Data.OscoinTx (Tx)
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
run :: (Serialise s, Serialise (Ledger.StateVal Crypto))
    => Int
    -> Node.Handle Crypto (Tx Crypto) s i
    -> IO ()
run port hdl =
    withLogging hdl $ \logging ->
        runApi (api logging) port hdl

app :: ( Typeable c
       , FromHttpApiData (BlockHash c)
       , Serialise (BlockHash c)
       , Serialise (SealedBlock c tx s)
       , Buildable (Crypto.Hash c)
       , ApiTx c tx
       )
    => Node.Handle c tx s i
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
       , Serialise (BlockHash c)
       , Serialise (SealedBlock c tx s)
       , Buildable (Crypto.Hash c)
       , ApiTx c tx
       )
    => Wai.Middleware
    -> Api c tx s i ()
api mdlware = do
    middleware $ mdlware
               . Wai.staticPolicy staticFilePolicy

    -- / ----------------------------------------------------------------------

    get root Handlers.root

    -- /blocks/:id ------------------------------------------------------------

    get ("blocks" <//> var) Handlers.getBlock

    -- /blocks/by-height ------------------------------------------------------

    get ("blocks" <//> "by-height") Handlers.lookupBlocksByHeight

    -- /blocks/hashes ------------------------------------------------------

    get ("blocks" <//> "hashes") Handlers.lookupHashesByHeight

    -- /blocks/by-height/:height ----------------------------------------------

    get ("blocks" <//> "by-height" <//> var) Handlers.lookupBlockByHeight

    -- /blockchain/tip -------------------------------------------------------

    get ("blockchain" <//> "tip") Handlers.getTip

    -- /blockchain/best -------------------------------------------------------

    get ("blockchain" <//> "best") Handlers.getBestChain

    -- /transactions ----------------------------------------------------------

    post "transactions" Handlers.submitTransaction

    -- /state/:key ------------------------------------------------------------

    get ("state" <//> wildcard) Handlers.getStateValue


-- Internal --------------------------------------------------------------------

withLogging :: Node.Handle c tx s i -> (Wai.Middleware -> IO a) -> IO a
withLogging hdl action =
    action . loggingMiddleware . Node.cfgTelemetry $ Node.hConfig hdl
