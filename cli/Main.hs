module Main (main) where

import           Oscoin.Prelude

import           Oscoin.CLI
import           Oscoin.CLI.Command.Result (printResult)

main :: IO ()
main = do
    cmd <- execParser
    result <- runCommand cmd
    printResult result
