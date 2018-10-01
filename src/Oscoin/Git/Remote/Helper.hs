module Oscoin.Git.Remote.Helper
    ( runRemoteHelper
    ) where

import           Oscoin.Prelude

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import qualified Data.Text as T
import           Oscoin.Git.Remote.GitArgs
import           Oscoin.Git.Remote.GitCmd
import           Oscoin.Git.Remote.IOUtil
import           System.IO (Handle)
import qualified System.IO as SIO
import qualified System.Process.Typed as P

--------------------------------------------------------------------------------

-- TODO: map hash/id to url by selecting the best remote from a list

eval :: Text -> P.Process SIO.Handle stdout stderr -> GitCmd -> IO ()
eval _line1 _p GOtherwise =
    pure ()
eval line1 p cmd = do
    rest <- getLines
    let lines = line1 : rest
    a1 <- async $ execHook lines p cmd
    flushLinesToProc rest p
    _ <- wait a1
    flushLinesToProc ["\n"] p

execHook :: [Text] -> P.Process SIO.Handle stdout stderr -> GitCmd -> IO ()
execHook lines _p GFetch =
    logLinesErr "fetch args:" lines
execHook lines _p GPush =
    logLinesErr "push args:" lines
execHook _lines _p _cmd = pure ()

logLinesErr :: Text -> [Text] -> IO ()
logLinesErr prefix lines = do
    hPutStrLn stderr $ T.unpack prefix
    forM_ lines $
      \line -> do
        hPutStrLn stderr line
        threadDelay 125000      -- WIP artificial delay

flushLinesToProc :: [Text] -> P.Process Handle stdout stderr -> IO ()
flushLinesToProc lines p =
    forM_ lines $
        \line -> do
          hPutStrLn (P.getStdin p) line
          SIO.hFlush (P.getStdin p)

spawnProc :: Text -> Text -> IO ()
spawnProc remoteName url = do
    let config = P.setStdin  P.createPipe
               $ P.setStdout P.inherit
               $ P.setStderr P.inherit
               $ P.proc (helperCmd url) [T.unpack remoteName, T.unpack url]

    P.withProcess_ config $ \p -> forever $ do
        line <- getLine
        flushLinesToProc [line] p
        eval line p $ parseGitCmd line

-- TODO: support more protocols and do not hard-code /usr/lib/git-core/
helperCmd :: Text -> String
helperCmd url
    | "https" `T.isPrefixOf` url = "/usr/lib/git-core/git-remote-https"
    | otherwise                  = "/usr/lib/git-core/git-remote-http"

runRemoteHelper :: IO ()
runRemoteHelper = do
    args <- parseArgs
    configureStdIO
    spawnProc (gitArgRemoteName args) (gitArgUrl args)
