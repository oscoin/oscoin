-- | A module to test long-running processes.
module Test.Sandbox
    ( SandboxOptions(..)
    , defaultSandboxOptions
    , withSandbox
    ) where

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception (bracket, finally)
import           Control.Monad (fail)

import           Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process

data SandboxOptions = SandboxOptions {
    aliveThreshold :: Int
    -- ^ How many seconds to wait for a process to be up and running.
  }

defaultSandboxOptions :: SandboxOptions
defaultSandboxOptions = SandboxOptions { aliveThreshold = 1 }

withSandbox :: String
            -> [String]
            -> SandboxOptions
            -> (Handle -> Handle -> IO ())
            -> IO ()
withSandbox processName args opts assertion =
    bracket acquire release $ \(procStdOut, procStdErr, procHandle) -> do
        -- First we wait up to 'aliveThreshold' to see if the process exited
        -- or is still running
        a1 <- async $ threadDelay (aliveThreshold opts * 1000000)
        a2 <- async $ waitForProcess procHandle
        res <- waitEitherCatchCancel a1 a2
        case res of
            Left (Left ex)  -> fail (show ex)
            Left (Right ()) ->
                -- The process is still running, we can now check some
                -- properties on it.
                assertion procStdOut procStdErr
            Right (Left ex) -> fail (show ex)
            Right (Right (ExitFailure x)) -> do
                stdErrLog <- T.hGetContents procStdErr
                -- Read everything from stderr and try to inform the user
                -- on what went wrong.
                fail $  "The process exited with error code = "
                     <> show x
                     <> " and the following message:\n"
                     <> T.unpack stdErrLog
            Right (Right ExitSuccess) -> pure ()

   where
       redirectStdOut :: FilePath -> String
       redirectStdOut tmpFile = " 2>" <> tmpFile

       redirectStdErr :: FilePath -> String
       redirectStdErr tmpFile = " 1>" <> tmpFile

       acquire :: IO (Handle, Handle, ProcessHandle)
       acquire = do
         stdOutFile <- writeSystemTempFile "Test.Sandbox.StdOut" mempty
         stdErrFile <- writeSystemTempFile "Test.Sandbox.StdErr" mempty
         let cmd =  processName
                 <> " "
                 <> mconcat (intersperse " " args)
                 <> redirectStdOut stdOutFile
                 <> redirectStdErr stdErrFile
         (_, _, _, processHandle) <- createProcess $
             (shell cmd) { create_group = True -- Crucial for clean shutdown.
                         , std_in  = NoStream
                         , std_out = NoStream
                         , std_err = NoStream
                         }
         stdOutHandle <- openFile stdOutFile ReadMode
         stdErrHandle <- openFile stdOutFile ReadMode
         pure (stdOutHandle, stdErrHandle, processHandle)

       release :: (Handle, Handle, ProcessHandle) -> IO ()
       release (stdOutHandle, stdErrHandle, ph) =
           hClose stdOutHandle `finally`
           hClose stdErrHandle `finally`
           interruptProcessGroupOf ph
