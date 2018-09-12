module Oscoin.Test.CLI
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.CLI
import qualified Oscoin.CLI.Radicle as Rad
import           Oscoin.Data.Tx (txMessageContent)

import           Oscoin.Test.CLI.Helpers

import           Test.Tasty
import           Test.Tasty.HUnit


tests :: [TestTree]
tests =
    [ testRevisionCreate
    , testGenerateKeyPair
    ]

testRevisionCreate :: TestTree
testRevisionCreate = testCase "revision create" $ do
    (result, TestCommandState{..}) <- runCLI ["create"]
    let submittedMsg = txMessageContent $ head submittedTransactions
    let expectedMessage = Rad.fnApply "create-revision" [Rad.toRadicle emptyRevision]
    assertEqual "Expected message to be an empty revision" expectedMessage submittedMsg
    assertResultValue result

testGenerateKeyPair :: TestTree
testGenerateKeyPair = testCase "generate-keypair" $ do
    let setNoKeyPair s = s { storedKeyPair = Nothing }
    (result, TestCommandState{..}) <- runCLIWithState ["generate-keypair"] setNoKeyPair
    assertBool "No keypair was stored" $ isJust storedKeyPair
    assertResultOk result
