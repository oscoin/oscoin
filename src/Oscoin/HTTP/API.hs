module Oscoin.HTTP.API where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.Environment

import qualified Oscoin.HTTP.Handlers as Handlers
import           Oscoin.HTTP.Internal

import           Data.Aeson (object, (.=), FromJSON, ToJSON)
import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai
import           Data.Typeable (Typeable)

-- TODO: Don't import this here? Create a HTTP.Routing module?
import           Web.Spock (get, json, middleware, post, root, var, (<//>))

withAPI
    :: (Typeable tx, FromJSON tx, ToJSON tx, Hashable tx)
    => Environment -> (Api tx s i () -> m a) -> m a
withAPI env f = f (api env)

-- | Entry point for API.
api :: (Typeable tx, FromJSON tx, ToJSON tx, Hashable tx)
    => Environment
    -> Api tx s i ()
api env = do
    middleware $ loggingMiddleware env
               . Wai.staticPolicy (Wai.noDots >-> Wai.addBase ".")

    -- / ----------------------------------------------------------------------

    get root $
        json $ object [ "ok" .= True ]

    -- /node/mempool ----------------------------------------------------------

    get  ("node" <//> "mempool") Handlers.getAllTransactions
    post ("node" <//> "mempool") Handlers.submitTransaction

    -- /node/mempool/:id ------------------------------------------------------

    get ("node" <//> "mempool" <//> var) Handlers.getTransaction
