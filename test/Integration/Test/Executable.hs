module Integration.Test.Executable
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS

import           Network.HTTP.Client
                 ( defaultManagerSettings
                 , httpLbs
                 , newManager
                 , parseRequest
                 , responseBody
                 , responseStatus
                 )
import           Network.HTTP.Types (statusCode)
import           System.IO (Handle)
import           System.IO.Temp
import           System.Random (getStdGen, randomRIO, randomRs)

import           Test.Sandbox
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Integration.Test.Executable" testSmoke

data Ports = Ports
    { gossipPort  :: Int
    , apiPort     :: Int
    , ekgPort     :: Int
    , metricsPort :: Int
    }

randomPorts :: IO Ports
randomPorts = Ports
    <$> randomRIO (6000, 7990)
    <*> randomRIO (8000, 8990)
    <*> randomRIO (9000, 10000)
    <*> randomRIO (10000, 11990)

withOscoinExe :: Ports -> (Handle -> Handle -> Assertion) -> Assertion
withOscoinExe Ports{..} f = do
    randomNetwork <- take 63 . randomRs ('a', 'z') <$> getStdGen

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
                                 , "--metrics-host"
                                 , "127.0.0.1"
                                 , "--metrics-port"
                                 , show metricsPort
                                 , "--ekg-host"
                                 , "127.0.0.1"
                                 , "--ekg-port"
                                 , show ekgPort
                                 , "--blockstore"
                                 , dbPath
                                 , "--genesis"
                                 , "data/genesis.yaml"
                                 ] defaultSandboxOptions $
                \stdoutHandle stdErrHandle -> f stdoutHandle stdErrHandle

testSmoke :: Assertion
testSmoke = do
    ports@Ports { apiPort, ekgPort, metricsPort } <- randomPorts
    withOscoinExe ports $ \stdoutHdl _ -> do
        out <- C8.hGet stdoutHdl 2048
        expectOutputContains "node starting" out
        expectOutputContains "mined block"   out

        mgr <- newManager defaultManagerSettings
        -- API root is actually a CBOR encoded 'Nothing :: Maybe ()'. lol
        checkEndpoint mgr (mkUrl apiPort     mempty    ) 200 noEmptyBody
        checkEndpoint mgr (mkUrl ekgPort     mempty    ) 200 noEmptyBody
        checkEndpoint mgr (mkUrl metricsPort "/metrics") 200 noEmptyBody
        checkEndpoint mgr (mkUrl metricsPort "/healthz") 200 emptyBody
  where
    expectOutputContains expected actual =
        assertBool ("Expected `" <> toS expected <> "` in output: " <> toS actual) $
            expected `C8.isInfixOf` actual

    noEmptyBody url bdy =
        assertBool (url <> ": Empty response") $
            C8.length bdy > 0

    emptyBody url bdy =
        assertBool (url <> ": Non-empty response: " <> toS bdy) $
            C8.length bdy == 0

    mkUrl port path = "http://127.0.0.1:" <> show port <> path

    checkEndpoint mgr url code assertBody = do
        rq <- parseRequest url
        rs <- httpLbs rq mgr
        let
            rsStatus = statusCode  $ responseStatus rs
            rsBody   = LBS.toStrict $ responseBody   rs
         in do
            assertEqual (url <> ": Unexpected status code") code rsStatus
            assertBody url rsBody
