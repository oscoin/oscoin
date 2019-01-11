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
    , assertFileExists
    , assertFileNotExists
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Client
import           Oscoin.API.Types hiding (Result)
import           Oscoin.CLI
import           Oscoin.CLI.KeyStore
import           Oscoin.CLI.Spinner (newTestSpinner)
import           Oscoin.Crypto.Hash (Hashed, hash)
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Time as Time

import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Options.Applicative as Options
import qualified System.Directory as Dir
import           System.Environment
import           System.IO.Temp (withSystemTempDirectory)

import           Test.Tasty.HUnit.Extended


-- | Run the CLI in a test environment and print the result and the
-- final TestCommandState
runCLI :: [String] -> IO TestCommandState
runCLI args = runCLIWithState args identity

-- | Same as @runCLI@ but accepts an additional function that allows
-- you to modify the initial state of the test environment before
-- running the command.
runCLIWithState
    :: [String] -> (TestCommandState -> TestCommandState) -> IO TestCommandState
runCLIWithState args setupState = do
    storedKeyPair <- Crypto.generateKeyPair
    cmd <- case execParserPure args of
        Options.Success cmd -> pure $ cmd
        Options.CompletionInvoked _ -> assertFailure "Unexpected CLI completion invoked"
        Options.Failure failure ->
            let failureMessage = fst $ Options.renderFailure failure ""
            in assertFailure $ "Failed to parse CLI arguments: \n" <> failureMessage
    let initialState = TestCommandState { transactions = mempty
                                        , storedKeyPair = Just storedKeyPair
                                        , commandOutput = ""
                                        }
    (result, cliState) <- runStateT
        (modify setupState >> fromTestCommandRunner (dispatchCommand cmd))
        initialState
    assertResultOk result
    pure cliState

-- | Monad to run the CLI for testing purposes. It mocks both the API
-- client and the Key Store by providing `MonadClient` and
-- `MonadKeyStore` instances.
newtype TestCommandRunner a = TestCommandRunner { fromTestCommandRunner :: StateT TestCommandState IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState TestCommandState)

data TestCommandState = TestCommandState
    { transactions  :: Map.Map (Hashed RadTx) RadTx
    , storedKeyPair :: Maybe (Crypto.PublicKey, Crypto.PrivateKey)
    , commandOutput :: Text
    }

instance MonadCLI TestCommandRunner where
    sleep _ = pure ()
    putLine t = putStr (t <> "\n")
    putString t = modify $ \s -> s { commandOutput = commandOutput s <> t }
    withSpinner _ _ = (newTestSpinner >>=)
    progress _ _ = pure ()
    readRadFile _ = pure $ Left "can't read file in tests"
    getTime = pure Time.epoch


instance MonadRandom TestCommandRunner where
    getRandomBytes = liftIO . getRandomBytes

instance MonadKeyStore TestCommandRunner where
    writeKeyPair _ kp = modify (\s -> s { storedKeyPair = Just kp  })
    readKeyPair _ = do
        TestCommandState{..} <- get
        case storedKeyPair of
            Just kp -> pure $ kp
            Nothing -> panic "No keypair stored"

instance MonadClient TestCommandRunner where
    submitTransaction tx = do
        modify (\s -> s { transactions = Map.insert (hash tx) tx (transactions s) })
        pure $ Ok $ TxSubmitResponse $ hash tx

    getTransaction _txHash = Map.lookup _txHash <$> gets transactions >>= \case
        Nothing -> pure $ Err "not found"
        Just tx -> pure $ Ok TxLookupResponse
            { txHash = _txHash
            , txBlockHash = Nothing
            , txOutput = Nothing
            , txConfirmations = 1
            , txPayload = tx
            }

    getState _key =
        pure $ Err "state not found"

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
