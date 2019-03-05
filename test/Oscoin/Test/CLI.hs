{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.CLI
    ( tests
    ) where

import           Oscoin.Prelude

import           Database.SQLite.Simple.Orphans ()
import           Oscoin.CLI
import           Oscoin.CLI.KeyStore
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txMessageContent)
import           Oscoin.Test.CLI.Helpers
import           Oscoin.Test.Util (Condensed(..))

-- For the CLI, at the moment, we can test only against the Crypto.
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Hash.RealWorld ()

import           Radicle.Conversion
import qualified Radicle.Extended as Rad

import qualified Data.Map.Strict as Map
import           System.FilePath ((</>))

import           Test.Tasty
import           Test.Tasty.HUnit.Extended


tests :: [TestTree]
tests =
    [ testRevisionCreate
    , testGenerateKeyPair
    , testKeyStore
    ]

testRevisionCreate :: TestTree
testRevisionCreate = testCase "revision create" $ do
    cliState  <- runCLI @Crypto ["revision", "create", "--confirmations=1"]
    let submittedMsg = txMessageContent . snd <$> Map.lookupMin (transactions cliState)
    let expectedMessage = Rad.fnApply "create-revision" [toRad emptyRevision]
    assertEqual "Expected message to be an empty revision"
                (Just expectedMessage)
                submittedMsg

testGenerateKeyPair :: TestTree
testGenerateKeyPair = testCase "keypair generate" $ do
    let setNoKeyPair s = s { storedKeyPair = Nothing }
    TestCommandState{..} <- runCLIWithState @Crypto ["keypair", "generate"] setNoKeyPair
    assertBool "No keypair was stored" $ isJust storedKeyPair

testKeyStore :: TestTree
testKeyStore = testCase "file keystore read/write" $
    withSandboxHome $ \home -> do
        let skPath = home </> ".config" </> "oscoin" </> "id.key"
        let pkPath = home </> ".config" </> "oscoin" </> "id.pub"
        assertFileNotExists skPath
        assertFileNotExists pkPath

        kp <- Crypto.generateKeyPair
        flip runReaderT (Nothing :: Maybe FilePath) $ do
            writeKeyPair kp
            liftIO $ assertFileExists skPath
            liftIO $ assertFileExists pkPath

            kp' <- readKeyPair
            condensed kp @=? condensed kp'
