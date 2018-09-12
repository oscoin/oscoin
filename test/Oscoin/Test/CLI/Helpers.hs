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

    -- * Assertions
    , assertResultOk
    , assertResultValue
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Client
import           Oscoin.API.Types hiding (Result)
import           Oscoin.CLI
import           Oscoin.CLI.KeyStore
import           Oscoin.CLI.Command.Result
import           Oscoin.Crypto.Hash (hash)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Node (Receipt(..))

import           Control.Monad.State
import qualified Options.Applicative as Options

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
    , storedKeyPair :: Maybe (Crypto.PublicKey, Crypto.PrivateKey)
    }

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

assertResultValue :: (Show a) => Result a -> Assertion
assertResultValue (ResultValue _) = pure ()
assertResultValue result = assertFailure $ "Expected ResultValue, got " <> show result

assertResultOk :: (Show a) => Result a -> Assertion
assertResultOk ResultOk = pure ()
assertResultOk result = assertFailure $ "Expected ResultOk, got " <> show result
