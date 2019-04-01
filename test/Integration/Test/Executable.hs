module Integration.Test.Executable
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Data.ByteString.Char8 as C8

import           Network.HTTP.Client
                 ( defaultManagerSettings
                 , httpNoBody
                 , newManager
                 , parseRequest
                 , responseStatus
                 )
import           Network.HTTP.Types (statusCode)
import           System.IO (Handle)
import           System.IO.Temp
import           System.Random (getStdGen, randomRIO, randoms)

import           Test.Sandbox
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Integration.Test.Executable"
    [ testCase "testStartsOK"  testStartsOK
    , testCase "testMinerOK"   testMinerOK
    , testCase "testMetricsOK" testMetricsOK
    ]

data Ports = Ports
    { gossipPort :: Int
    , apiPort    :: Int
    , ekgPort    :: Int
    }

randomPorts :: IO Ports
randomPorts = Ports
    <$> randomRIO (6000, 7990)
    <*> randomRIO (8000, 8990)
    <*> randomRIO (9000, 10000)

withOscoinExe :: Ports -> (Handle -> Handle -> Assertion) -> Assertion
withOscoinExe Ports{..} f = do
    randomNetwork <- take 63 . randoms <$> getStdGen

    -- Generates a temporary directory where to store some ephemeral keys, which
    -- are needed for the test to pass on CI.
    withSystemTempDirectory "Oscoin.Ephemeral.Keys" $ \keyPath ->
        withSystemTempFile "blockstore.db" $ \dbPath _ -> do
            -- First, generate the keys with the CLI
            withSandbox "oscoin-cli" ["keypair", "generate", "--keys", keyPath] defaultSandboxOptions $
                \_ _ -> pure ()
            -- Then, run the test.
            withSandbox "oscoin" [ "--api-port"
                                 , show apiPort
                                 , "--gossip-port"
                                 , show gossipPort
                                 , "--keys"
                                 , keyPath
                                 , "--network"
                                 , randomNetwork
                                 , "--seed"
                                 , "127.0.0.1:" <> show gossipPort
                                 , "--ekg-port"
                                 , show ekgPort
                                 , "--blockstore"
                                 , dbPath
                                 ] defaultSandboxOptions $
                \stdoutHandle stdErrHandle -> f stdoutHandle stdErrHandle

testStartsOK :: Assertion
testStartsOK = do
    ports <- randomPorts
    withOscoinExe ports $ \stdoutHandle _stdErrHandle -> do
        -- TODO(adn) In the future we want a better handshake string here.
        actual <- C8.hGet stdoutHandle 100
        assertBool ("oscoin started but gave unexpected output: " <> C8.unpack actual)
                   ("running in" `C8.isInfixOf` actual)

testMinerOK :: Assertion
testMinerOK = do
    ports <- randomPorts
    withOscoinExe ports $ \stdoutHandle _stdErrHandle -> do
        actual <- C8.hGet stdoutHandle 2000
        assertBool ("oscoin started but gave unexpected output: " <> C8.unpack actual)
                   ("mined block" `C8.isInfixOf` actual)

testMetricsOK :: Assertion
testMetricsOK = do
    ports <- randomPorts
    withOscoinExe ports $ \_ _ -> do
        mgr <- newManager defaultManagerSettings
        req <- parseRequest $ "HEAD http://127.0.0.1:" <> show (ekgPort ports)
        res <- statusCode . responseStatus <$> httpNoBody req mgr
        assertEqual "Unexpected status code" 200 res
