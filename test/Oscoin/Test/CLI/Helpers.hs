-- | This module provides the 'runCLI' and 'runCLIWithState' functions
-- to run the CLI in a test environment. The test environment is a
-- state monad that mocks the API client and the Key Store.
--
-- The module also exports assertions to test CLI behavior.
module Oscoin.Test.CLI.Helpers
    (
    -- * Run the CLI
      runCLI
    , runCLIWithState
    , TestCommandState(..)

    -- * Sandbox
    , withSandboxHome

    -- * Assertions
    , assertResultOk
    , assertResultValue
    , assertFileExists
    , assertFileNotExists
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Client
import           Oscoin.API.Types hiding (Result)
import           Oscoin.CLI
import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.KeyStore
import           Oscoin.Crypto.Hash (hash)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Node (Receipt(..))

import           Control.Exception.Safe
import           Control.Monad.State
import           Crypto.Random.Types (MonadRandom(..))
import qualified Options.Applicative as Options
import qualified System.Directory as Dir
import           System.Environment
import           System.IO.Temp (withSystemTempDirectory)

import           Test.Tasty.HUnit


-- | Run the CLI in a test environment and print the result and the
-- final TestCommandState
runCLI :: [String] -> IO (Result Text, TestCommandState)
runCLI args = runCLIWithState args identity

-- | Same as @runCLI@ but accepts an additional function that allows
-- you to modify the initial state of the test environment before
-- running the command.
runCLIWithState
    :: [String] -> (TestCommandState -> TestCommandState) -> IO (Result Text, TestCommandState)
runCLIWithState args setupState = do
    storedKeyPair <- Crypto.generateKeyPair
    cmd <- case execParserPure args of
        Options.Success cmd -> pure $ cmd
        Options.CompletionInvoked _ -> assertFailure "Unexpected CLI completion invoked"
        Options.Failure failure ->
            let failureMessage = fst $ Options.renderFailure failure ""
            in assertFailure $ "Failed to parse CLI arguments: \n" <> failureMessage
    let initialState = TestCommandState { submittedTransactions = []
                                        , storedKeyPair = Just storedKeyPair
                                        }
    runStateT
        (modify setupState >> fromTestCommandRunner (dispatchCommand cmd))
        initialState

-- | Monad to run the CLI for testing purposes. It mocks both the API
-- client and the Key Store by providing `MonadClient` and
-- `MonadKeyStore` instances.
newtype TestCommandRunner a = TestCommandRunner { fromTestCommandRunner :: StateT TestCommandState IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState TestCommandState)


data TestCommandState = TestCommandState
    { submittedTransactions :: [ RadTx ]
    , storedKeyPair         :: Maybe (Crypto.PublicKey, Crypto.PrivateKey)
    }

instance MonadRandom TestCommandRunner where
    getRandomBytes = io . getRandomBytes

instance MonadKeyStore TestCommandRunner where
    writeKeyPair kp = modify (\s -> s { storedKeyPair = Just kp  })
    readKeyPair = do
        TestCommandState{..} <- get
        case storedKeyPair of
            Just kp -> pure $ kp
            Nothing -> error "No keypair stored"

instance MonadClient TestCommandRunner where
    submitTransaction tx = do
        modify (\s -> s { submittedTransactions = tx : submittedTransactions s })
        pure $ Ok $ Receipt $ hash tx

    getTransaction _txHash =
        pure $ Err "transaction not found"

    getState _key =
        pure $ Err "state not found"

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
            case lookup key envBefore of
                Just v  -> setEnv key v
                Nothing -> unsetEnv key

----------------------------------------------------

assertResultValue :: (Show a) => Result a -> Assertion
assertResultValue (ResultValue _) = pure ()
assertResultValue result          = assertFailure $ "Expected ResultValue, got " <> show result

assertResultOk :: (Show a) => Result a -> Assertion
assertResultOk ResultOk = pure ()
assertResultOk result   = assertFailure $ "Expected ResultOk, got " <> show result

assertFileExists :: FilePath -> IO ()
assertFileExists path =
    Dir.doesFileExist path >>= assertBool ("Expected file " <> path <> " to exist")

assertFileNotExists :: FilePath -> IO ()
assertFileNotExists path =
    not <$> Dir.doesFileExist path >>= assertBool ("Expected file " <> path <> " to exist")

