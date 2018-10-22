module Main (main) where

import           Oscoin.Prelude

import           Oscoin.CLI

main :: IO ()
main = do
    cmd <- execParser
    result <- runCommand cmd
    case result of
        ResultOk      -> pure ()
        ResultError e -> die e
