module Integration.Test.Executable
    ( tests
    ) where

import           Oscoin.Prelude

import           Data.ByteString.Char8 as C8

import           System.IO (Handle)
import           System.IO.Temp
import           System.Random (randomRIO)

import           Test.Sandbox
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: [TestTree]
tests =
    [ testCase "it successfully starts" testStartsOK
    , testCase "the miner produces blocks" testMinerOK
    ]

withOscoinExe :: (Handle -> Handle -> Assertion) -> Assertion
withOscoinExe f = do
    randomGossipPort <- randomRIO (6000, 7990)
    randomEkgPort    <- randomRIO (8000, 8990)
    randomSpockPort  <- randomRIO (9000, 10000)

    -- Generates a temporary directory where to store some ephemeral keys, which
    -- are needed for the test to pass on CI.
    withSystemTempDirectory "Oscoin.Ephemeral.Keys" $ \keyPath -> do
        -- First, generate the keys with the CLI
        withSandbox "oscoin-cli" ["keypair", "generate", "--keys", keyPath] defaultSandboxOptions $
            \_ _ -> pure ()
        -- Then, run the test.
        withSandbox "oscoin" [ "--api-port"
                             , show (randomSpockPort :: Int)
                             , "--gossip-port"
                             , show (randomGossipPort :: Int)
                             , "--keys"
                             , keyPath
                             , "--seed"
                             , "127.0.0.1:" <> show (randomGossipPort :: Int)
                             , "--ekg-port"
                             , show (randomEkgPort :: Int)
                             ] defaultSandboxOptions $
            \stdoutHandle stdErrHandle -> f stdoutHandle stdErrHandle

testStartsOK :: Assertion
testStartsOK =
    withOscoinExe $ \stdoutHandle _stdErrHandle -> do
        -- TODO(adn) In the future we want a better handshake string here.
        actual <- C8.hGet stdoutHandle 100
        assertBool ("oscoin started but gave unexpected output: " <> C8.unpack actual)
                   ("running in" `C8.isInfixOf` actual)

testMinerOK :: Assertion
testMinerOK =
    withOscoinExe $ \stdoutHandle _stdErrHandle -> do
        actual <- C8.hGet stdoutHandle 2000
        assertBool ("oscoin started but gave unexpected output: " <> C8.unpack actual)
                   ("mined block" `C8.isInfixOf` actual)
