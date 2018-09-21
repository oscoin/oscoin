module Oscoin.Test.CLI
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.CLI
import           Oscoin.CLI.KeyStore
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txMessageContent)

import           Oscoin.Test.CLI.Helpers

import           Radicle.Conversion
import qualified Radicle.Extended as Rad

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
    cliState  <- runCLI ["revision", "create"]
    let submittedMsg = txMessageContent <$> head (submittedTransactions cliState)
    let expectedMessage = Rad.fnApply "create-revision" [toRadicle emptyRevision]
    assertEqual "Expected message to be an empty revision"
                (Just expectedMessage)
                submittedMsg

testGenerateKeyPair :: TestTree
testGenerateKeyPair = testCase "keypair generate" $ do
    let setNoKeyPair s = s { storedKeyPair = Nothing }
    TestCommandState{..} <- runCLIWithState ["keypair", "generate"] setNoKeyPair
    assertBool "No keypair was stored" $ isJust storedKeyPair

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
