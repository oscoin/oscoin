module Integration.Test.Executable
    ( tests
    ) where

import           Oscoin.Prelude

import           Data.ByteString.Char8 as C8

import           System.IO.Temp

import           Test.Sandbox
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: [TestTree]
tests =
    [ testCase "it successfully starts" testStartsOK ]


testStartsOK :: Assertion
testStartsOK =
    -- Generates a temporary directory where to store some ephimeral keys, which
    -- are needed for the test to pass on CI.
    withSystemTempDirectory "Oscoin.Ephimeral.Keys" $ \keyPath -> do
        -- First, generate the keys with the CLI
        withSandbox "oscoin-cli" ["keypair", "generate", "--keys", keyPath] defaultSandboxOptions $
            \_ _ -> pure ()
        -- Then, run the test.
        withSandbox "oscoin" ["--api-port", "9999", "--keys", keyPath] defaultSandboxOptions $
            \stdoutHandle _stdErrHandle -> do
                -- TODO(adn) In the future we want a better handshake string here.
                actual <- C8.hGet stdoutHandle 100
                assertBool ("oscoin started but gave unexpected output: " <> C8.unpack actual)
                           ("genesis is" `C8.isInfixOf` actual)
