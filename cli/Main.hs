module Main (main) where

import           Oscoin.Prelude

import           Oscoin.CLI (Command(..), runCommand, Options(..), defaultOptions)
import           Oscoin.CLI.Command.Result (printResult)

import           Oscoin.API.HTTP.Client (runHttpClientT)

import           System.Environment
import           System.Console.GetOpt
import           System.IO
import           System.Exit

options :: [OptDescr (Options -> Options)]
options =
    [ Option [] ["id"]
        (OptArg (\o opts -> opts { optsRevisionId = readStr <$> o })   "<revision-id>")     "Revision ID"
    ]

readCommand :: IO (Command, Options)
readCommand = do
    a <- getArgs
    case getOpt RequireOrder options a of
        (_, [], []) ->
            printUsage
        (flags, [command], []) ->
            pure (readStr command, foldr ($) defaultOptions flags)
        (_, _, msgs) -> do
            fatal ["oscoin-cli:", head msgs]
            printUsage
  where
    printUsage = do
        hPutStr stderr $ usageInfo (unlines
            [ "usage: oscoin-cli <command> [<args>]"
            , ""
            , "Revisions CLI"
            , ""
            , "commands:"
            , ""
            , "    create \t Create a new revision"
            , "    list   \t List known revisions"
            , ""
            , "args:"
            ]) options
        exitWith $ ExitFailure 1

fatal :: [String] -> IO ()
fatal = void . die . unwords

main :: IO ()
main = do
    (!cmd, opts) <- readCommand
    result <- runHttpClientT "http://127.0.0.1:8080" $ runCommand cmd opts
    printResult result
