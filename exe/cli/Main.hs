module Main (main) where

import           Oscoin.Prelude

import           Oscoin.CLI

main :: IO ()
main = do
    (mbKeysPath, cmd) <- execParser
    result <- runCommand mbKeysPath cmd
    case result of
        ResultOk      -> pure ()
        ResultError e -> die e
