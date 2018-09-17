module Oscoin.Test.CLI
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.CLI
import qualified Oscoin.CLI.Radicle as Rad
import           Oscoin.CLI.KeyStore
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txMessageContent)

import           Oscoin.Test.CLI.Helpers

import           System.FilePath ((</>))

import           Test.Tasty
import           Test.Tasty.HUnit


tests :: [TestTree]
tests =
    [ testRevisionCreate
    , testGenerateKeyPair
    , testKeyStore
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

testKeyStore :: TestTree
testKeyStore = testCase "file keystore read/write" $
    withSandboxHome $ \home -> do
        let skPath = home </> ".config" </> "oscoin" </> "id.key"
        let pkPath = home </> ".config" </> "oscoin" </> "id.pub"
        assertFileNotExists skPath
        assertFileNotExists pkPath

        kp <- Crypto.generateKeyPair
        writeKeyPair kp
        assertFileExists skPath
        assertFileExists pkPath

        kp' <- readKeyPair
        kp @=? kp'
