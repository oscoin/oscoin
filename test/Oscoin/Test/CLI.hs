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


runCommandTest :: Command -> Options -> IO (Result Text, TestApiClientState)
runCommandTest cmd opts =
    runStateT (runTestApiClient $ runCommand cmd opts) (TestApiClientState [])

assertResultValue :: (Show a) => Result a -> Assertion
assertResultValue (ResultValue _) = pure ()
assertResultValue result = assertFailure $ "Expected ResultValue, got " <> show result

testRevisionCreate :: TestTree
testRevisionCreate = testCase "revision create" $ do
    (result, TestApiClientState{..}) <- runCommandTest RevisionCreate defaultOptions
    let submittedMsg = txMessageContent $ head submitTransactionCalls
    let expectedMessage = Rad.fnApply "create-revision" [Rad.toRadicle emptyRevision]
    assertEqual "Expected message to be an empty revision" expectedMessage submittedMsg
    assertResultValue result