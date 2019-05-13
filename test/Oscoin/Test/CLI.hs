{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.CLI
    ( tests
    ) where

import           Oscoin.Prelude

import           Database.SQLite.Simple.Orphans ()
import           Oscoin.CLI.KeyStore
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Test.CLI.Helpers
import           Oscoin.Test.Crypto.PubKey.Arbitrary ()
import           Oscoin.Test.Util (Condensed(..))

-- For the CLI, at the moment, we can test only against the Crypto.
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Hash.RealWorld ()

import           System.FilePath ((</>))

import           Test.Tasty
import           Test.Tasty.HUnit.Extended


tests :: [TestTree]
tests =
    [ testGenerateKeyPair
    , testKeyStore
    ]

testGenerateKeyPair :: TestTree
testGenerateKeyPair = testCase "keypair generate" $ do
    let setNoKeyPair s = s { storedKeyPair = Nothing }
    TestCommandState{..} <- runCLI @Crypto ["keypair", "generate"] setNoKeyPair
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
