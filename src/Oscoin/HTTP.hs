{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.HTTP where

import           Oscoin.Prelude

import qualified Oscoin.HTTP.Handlers as Handlers
import           Oscoin.HTTP.Internal

import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson ((.=), object)
import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

run :: Int -> IO ()
run port = do
    spockCfg <- defaultSpockCfg () PCNoDatabase (State ())
    runSpock port (spock spockCfg app)

-- | Entry point for the Spock application.
app :: Api ()
app = do
    middleware $
        Wai.logStdoutDev . Wai.staticPolicy (Wai.noDots >-> Wai.addBase ".")

    get root $ do
        json $ object [ "ok" .= True ]

    -- /orgs/:org/data/:key

    get ("orgs" <//> var <//> "data" <//> var) Handlers.getOrgKey
    put ("orgs" <//> var <//> "data" <//> var) Handlers.setOrgKey
