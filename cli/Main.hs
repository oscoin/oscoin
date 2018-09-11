module Main (main) where

import           Oscoin.Prelude

import           Oscoin.CLI
import           Oscoin.CLI.Command.Result (printResult)
import           Oscoin.API.HTTP.Client (runHttpClientT)

main :: IO ()
main = do
    cmd <- execParser
    result <- runHttpClientT "http://127.0.0.1:8080" $ runCommand cmd
    printResult result
