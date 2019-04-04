module Main (main) where

import           Oscoin.Prelude

import           Oscoin.CLI
import           Oscoin.Configuration (getConfigPaths, keysDir)

main :: IO ()
main = do
    cfgPaths <- getConfigPaths
    CLI{..}  <- execParser cfgPaths
    result   <- runCommand (Just $ keysDir cliPaths) cliCommand
    case result of
        ResultOk      -> pure ()
        ResultError e -> die e
