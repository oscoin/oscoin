module Main (main) where

import           Oscoin.Prelude

import           Oscoin.CLI

main :: IO ()
main = do
    CLI{..} <- execParser
    result <- runCommand cliKeyPath cliCommand
    case result of
        ResultOk      -> pure ()
        ResultError e -> die e
