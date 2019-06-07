{-# LANGUAGE UndecidableInstances #-}
-- | This module provides the 'runCLI' function to run the CLI in a test
-- environment. The test environment is a state monad that mocks the API client
-- and the Key Store.
--
-- The module also exports assertions to test CLI behavior.
module Oscoin.Test.CLI.Helpers
    (
    -- * Run the CLI
      runCLI
    , TestCommandState(..)

    -- * Sandbox
    , withSandboxHome

    -- * Assertions
    , assertFileExists
    , assertFileNotExists
    ) where

import           Oscoin.Prelude

import           Oscoin.CLI
import           Oscoin.CLI.KeyStore
import qualified Oscoin.Configuration as Config
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx
import qualified Oscoin.Time as Time

import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Options.Applicative as Options
import qualified System.Directory as Dir
import           System.Environment
import           System.IO.Temp (withSystemTempDirectory)

import           Oscoin.Test.Crypto
import           Test.Tasty.HUnit.Extended


-- | Run the CLI in a test environment and print the result and the
-- final TestCommandState.
--
-- The function argument allows to modify the initial state of the test
-- environment before running the command.
runCLI
    :: IsCrypto c
    => [String]
    -> (TestCommandState c -> TestCommandState c)
    -> IO (TestCommandState c)
runCLI args setupState = do
    storedKeyPair <- Crypto.generateKeyPair
    cfgPaths      <- Config.getConfigPaths
    -- We don't seem to care about overriding the location of the keys for
    -- the 'TestCommandState', as the 'HOME' env var is overwritten with a
    -- throwaway temporary directory.
    cli <- case execParserPure cfgPaths args of
        Options.Success cli -> pure cli
        Options.CompletionInvoked _ -> assertFailure "Unexpected CLI completion invoked"
        Options.Failure failure ->
            let failureMessage = fst $ Options.renderFailure failure ""
            in assertFailure $ "Failed to parse CLI arguments: \n" <> failureMessage
    let initialState = TestCommandState { transactions = mempty
                                        , storedKeyPair = Just storedKeyPair
                                        , commandOutput = ""
                                        }
    (result, cliState) <- runStateT
        (modify setupState >> fromTestCommandRunner (dispatchCommand $ cliCommand cli))
        initialState
    assertResultOk result
    pure cliState

-- | Monad to run the CLI for testing purposes. It mocks both the API
-- client and the Key Store by providing `MonadClient` and
-- `MonadKeyStore` instances.
newtype TestCommandRunner c a = TestCommandRunner
    { fromTestCommandRunner :: StateT (TestCommandState c) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (TestCommandState c))

data TestCommandState c = TestCommandState
    { transactions  :: Map.Map (Hashed c (Tx c)) (Tx c)
    , storedKeyPair :: Maybe (Crypto.PublicKey c, Crypto.PrivateKey c)
    , commandOutput :: Text
    }

instance IsCrypto c => MonadCLI c (TestCommandRunner c) where
    sleep _       = pure ()
    putLine t     = putStr (t <> "\n")
    putString t   = modify $ \s -> s { commandOutput = commandOutput s <> t }
    getTime       = pure Time.epoch


instance MonadRandom (TestCommandRunner c) where
    getRandomBytes = liftIO . getRandomBytes

instance IsCrypto c => MonadKeyStore c (TestCommandRunner c) where
    keysPath = pure Nothing
    writeKeyPair kp = modify (\s -> s { storedKeyPair = Just kp  })
    readKeyPair = do
        TestCommandState{..} <- get
        case storedKeyPair of
            Just kp -> pure $ kp
            Nothing -> panic "No keypair stored"

----------------------------------------------------

assertResultOk :: Result -> Assertion
assertResultOk ResultOk = pure ()
assertResultOk (ResultError e) = assertFailure $ "Command resulted in error " <> show e

assertFileExists :: FilePath -> IO ()
assertFileExists path =
    Dir.doesFileExist path >>= assertBool ("Expected file " <> path <> " to exist")

assertFileNotExists :: FilePath -> IO ()
assertFileNotExists path =
    not <$> Dir.doesFileExist path >>= assertBool ("Expected file " <> path <> " to exist")

----------------------------------------------------

-- | Create a temporary directory, unset all environment variables and
-- set the @HOME@ environment variables to the temporary directory.
-- Passes the temporary home to the given action.
withSandboxHome :: (FilePath -> IO a) -> IO a
withSandboxHome run =
    withSystemTempDirectory "oscoin-cli-test" $ \tmpdir ->
        withEnv [("HOME", tmpdir)] (run tmpdir)

-- | Runs the given action with only the given environment varibles
-- set. Restores the previous environment when the action is done.
withEnv :: [(String, String)] -> IO a -> IO a
withEnv newEnv run =
    bracket prepareEnv restoreEnv $ const run
  where
    prepareEnv = do
        currentEnv <- getEnvironment
        forM_ currentEnv $ \(key, _) -> unsetEnv key
        forM_ newEnv $ uncurry setEnv
        pure $ currentEnv
    restoreEnv envBefore = do
        forM_ envBefore $ uncurry setEnv
        forM_ newEnv $ \(key, _) ->
            case List.lookup key envBefore of
                Just v  -> setEnv key v
                Nothing -> unsetEnv key
