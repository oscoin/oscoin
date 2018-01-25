{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.HTTP where

import           Oscoin.Prelude

import qualified Oscoin.HTTP.Handlers as Handlers
import           Oscoin.HTTP.Internal

import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson ((.=), object)
import qualified Network.Wai as Wai
import           Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

data Environment = Production | Development | Testing
    deriving (Show)

run :: Environment -> Int -> IO ()
run env port = do
    spockCfg <- defaultSpockCfg () PCNoDatabase (State ())
    runSpock port (spock spockCfg (app env))

loggingMiddleware :: Environment -> Wai.Middleware
loggingMiddleware Production = Wai.logStdout
loggingMiddleware Development = Wai.logStdoutDev
loggingMiddleware Testing = id

-- | Entry point for the Spock application.
app :: Environment -> Api ()
app env = do
    middleware $ loggingMiddleware env
               . Wai.staticPolicy (Wai.noDots >-> Wai.addBase ".")

    get root $ do
        json $ object [ "ok" .= True ]

    -- /orgs/:org/data/:key

    get ("orgs" <//> var <//> "data" <//> var) Handlers.getOrgKey
    put ("orgs" <//> var <//> "data" <//> var) Handlers.setOrgKey
