module Oscoin.Test.CLI
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.API.Client
import           Oscoin.API.Types hiding (Result)
import           Oscoin.CLI
import           Oscoin.CLI.Command.Result
import qualified Oscoin.CLI.Radicle as Rad
import           Oscoin.Crypto.Hash (hash)
import           Oscoin.Data.Tx (txMessageContent)
import           Oscoin.Node (Receipt(..))

import           Control.Monad.State
import qualified Options.Applicative as Options

import           Test.Tasty
import           Test.Tasty.HUnit


tests :: [TestTree]
tests =
    [ testRevisionCreate
    ]

newtype TestApiClient a = TestApiClient { runTestApiClient :: StateT TestApiClientState IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState TestApiClientState)

instance MonadClient TestApiClient where
    submitTransaction tx = do
        modify (\s -> s { submitTransactionCalls = tx : submitTransactionCalls s })
        pure $ Ok $ Receipt $ hash tx

    getTransaction _txHash =
        pure $ Err "transaction not found"

    getState _key =
        pure $ Err "state not found"


data TestApiClientState = TestApiClientState
    { submitTransactionCalls :: [ RadTx ] }


runTest :: [String] -> IO (Result Text, TestApiClientState)
runTest args = do
    cmd <- case execParserPure args of
        Options.Success cmd -> pure $ cmd
        Options.CompletionInvoked _ -> assertFailure "Unexpected CLI completion invoked"
        Options.Failure failure ->
            let failureMessage = fst $ Options.renderFailure failure ""
            in assertFailure $ "Failed to parse CLI arguments: \n" <> failureMessage
    runStateT (runTestApiClient $ runCommand cmd ) (TestApiClientState [])

assertResultValue :: (Show a) => Result a -> Assertion
assertResultValue (ResultValue _) = pure ()
assertResultValue result = assertFailure $ "Expected ResultValue, got " <> show result

testRevisionCreate :: TestTree
testRevisionCreate = testCase "revision create" $ do
    (result, TestApiClientState{..}) <- runTest ["create"]
    let submittedMsg = txMessageContent $ head submitTransactionCalls
    let expectedMessage = Rad.fnApply "create-revision" [Rad.toRadicle emptyRevision]
    assertEqual "Expected message to be an empty revision" expectedMessage submittedMsg
    assertResultValue result
