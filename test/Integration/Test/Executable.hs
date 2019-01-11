module Integration.Test.Executable
    ( tests
    ) where

import           Oscoin.Prelude

import           Data.ByteString.Char8 as C8

import           Test.Sandbox
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: [TestTree]
tests =
    [ testCase "it successfully starts" testStartsOK ]


testStartsOK :: Assertion
testStartsOK = withSandbox "oscoin" ["--api-port", "9999"] defaultSandboxOptions $
    \stdoutHandle _stdErrHandle -> do
        -- TODO(adn) In the future we want a better handshake string here.
        actual <- C8.hGet stdoutHandle 100
        assertBool ("oscoin started but gave unexpected output: " <> C8.unpack actual)
                   ("genesis is" `C8.isInfixOf` actual)
