-- | A module to test long-running processes.
module Test.Sandbox
    ( SandboxOptions(..)
    , defaultSandboxOptions
    , withSandbox
    ) where

import           Control.Monad (fail)
import           Oscoin.Prelude

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
            -> (FilePath -> IO ())
            -> IO ()
withSandbox processName args opts assertion =
    bracket acquire release $ \(procStdOutAsFile, procHandle) -> do
        -- First we wait up to 'aliveThreshold' to see if the process exited
        -- or is still running
        a1 <- async $ threadDelay (aliveThreshold opts * 1000000)
        a2 <- async $ waitForProcess procHandle
        res <- waitEitherCatchCancel a1 a2
        case res of
            Left (Left ex)  -> fail (show ex)
            Left (Right ()) -> do
                -- The process is still running, we can now check some
                -- properties on it.
                assertion procStdOutAsFile
            Right (Left ex) -> fail (show ex)
            Right (Right (ExitFailure x)) ->
                fail $ "The process exited with error code = " <> show x
            Right (Right ExitSuccess) -> return ()

   where
       redirectToTempFile :: FilePath -> String
       redirectToTempFile tmpFile = " 2>" <> tmpFile

       acquire :: IO (FilePath, ProcessHandle)
       acquire = do
         tmpFile <- writeSystemTempFile "Test.Sandbox" mempty
         let cmd =  processName
                 <> " "
                 <> mconcat (intersperse " " args)
                 <> redirectToTempFile tmpFile
         (_, _, _, processHandle) <- createProcess $
             (shell cmd) { create_group = True -- Crucial for clean shutdown.
                         , std_in  = NoStream
                         , std_out = NoStream
                         , std_err = NoStream
                         }
         return (tmpFile, processHandle)

       release :: (FilePath, ProcessHandle) -> IO ()
       release (_, ph) = interruptProcessGroupOf ph
