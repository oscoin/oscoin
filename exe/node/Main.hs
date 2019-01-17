module Main (main) where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.API.HTTP as HTTP
import           Oscoin.CLI.KeyStore (readKeyPair)
import           Oscoin.CLI.Parser (keyPathParser)
import qualified Oscoin.Consensus as Consensus
import qualified Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain (fromGenesis)
import           Oscoin.Crypto.Blockchain.Block (Block)
import           Oscoin.Crypto.Blockchain.Eval (evalBlock, fromEvalError)
import           Oscoin.Data.RadicleTx (RadTx)
import qualified Oscoin.Data.RadicleTx as Rad (pureEnv, txEval)
import           Oscoin.Environment (Environment(Development), toText)
import           Oscoin.Node (runNodeT, withNode)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.P2P (mkNodeId, runGossipT, withGossip)
import qualified Oscoin.P2P as P2P
import qualified Oscoin.P2P.Handshake as Handshake
import           Oscoin.Storage (hoistStorage)
import qualified Oscoin.Storage.Block as BlockStore
import qualified Oscoin.Storage.Block.STM as BlockStore
import qualified Oscoin.Storage.State as StateStore
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (withStdLogger)
import qualified Oscoin.Telemetry.Logging as Log
import           Oscoin.Telemetry.Metrics

import qualified Control.Concurrent.Async as Async
import qualified Data.Yaml as Yaml
import           GHC.Generics (Generic)
import           Network.Socket (HostName, PortNumber)

import           Options.Applicative

data Args = Args
    { host          :: HostName
    , gossipPort    :: PortNumber
    , apiPort       :: PortNumber
    , seeds         :: FilePath
    , genesis       :: FilePath
    , noEmptyBlocks :: Bool
    , keysPath      :: Maybe FilePath
    , ekgHost       :: HostName
    , ekgPort       :: PortNumber
    } deriving (Generic, Show)

args :: ParserInfo Args
args = info (helper <*> parser) $ progDesc "Oscoin Node"
  where
    parser = Args
        <$> option str
            ( short 'h'
           <> long "host"
           <> help "Host name to bind to for gossip"
           <> value "127.0.0.1"
           <> showDefault
            )
        <*> option auto
            ( long "gossip-port"
           <> help "Port number to bind to for gossip"
           <> value 6942
           <> showDefault
            )
        <*> option auto
            ( long "api-port"
           <> help "Port number to bind to for the HTTP API"
           <> value 8477
           <> showDefault
            )
        <*> option str
            ( long "seeds"
           <> help "Path to YAML file describing gossip seed nodes"
           <> value "exe/node/gossip-seeds.yaml"
            )
        <*> option str
            ( long "genesis"
           <> help "Path to genesis file"
           <> value "data/genesis.yaml"
            )
        <*> switch
            ( long "no-empty-blocks"
           <> help "Do not generate empty blocks"
           <> showDefault
            )
        <*> keyPathParser
        <*> option str
            ( long "ekg-host"
           <> help "Host name to bind to for the EKG server"
           <> value "0.0.0.0"
           <> showDefault
            )
        <*> option auto
            ( long "ekg-port"
           <> help "Port number to bind to for the EKG server"
           <> value 8090
           <> showDefault
            )


main :: IO ()
main = do
    Args{..} <- execParser args

    let consensus = Consensus.nakamotoConsensus
    let env       = Development

    keys        <- runReaderT readKeyPair keysPath
    nid         <- pure (mkNodeId $ fst keys)
    mem         <- Mempool.newIO
    gen         <- Yaml.decodeFileThrow genesis :: IO (Block RadTx Nakamoto.PoW)
    genState    <- either (die . fromEvalError) pure (evalBlock Rad.txEval Rad.pureEnv gen)
    stStore     <- StateStore.fromStateM genState
    blkStore    <- BlockStore.newIO $ BlockStore.initWithChain (fromGenesis gen)
    seeds'      <- Yaml.decodeFileThrow seeds
    config      <- Consensus.getConfig env

    metricsStore <- newMetricsStore $ labelsFromList [("env", toText env)]
    forkEkgServer metricsStore ekgHost ekgPort

    withStdLogger  Log.defaultConfig { Log.cfgLevel = Log.Debug } $ \lgr ->
        let telemetryStore = Telemetry.newTelemetryStore lgr metricsStore
        in withNode (mkNodeConfig env telemetryStore noEmptyBlocks config)
                    nid
                    mem
                    stStore
                    blkStore
                    Rad.txEval
                    consensus                                      $ \nod ->
        withGossip telemetryStore
                   P2P.NodeAddr { P2P.nodeId   = nid
                                , P2P.nodeHost = host
                                , P2P.nodePort = gossipPort
                                }
                   seeds'
                   (storage nod config)
                   (Handshake.simpleHandshake keys)               $ \gos ->
            Async.runConcurrently $
                     Async.Concurrently (HTTP.run (fromIntegral apiPort) Development nod)
                  <> Async.Concurrently (miner nod gos)
  where
    mkNodeConfig env telemetryStore neb config = Node.Config
        { Node.cfgEnv = env
        , Node.cfgTelemetryStore = telemetryStore
        , Node.cfgNoEmptyBlocks = neb
        , Node.cfgConsensusConfig = config
        }

    miner nod gos = runGossipT gos . runNodeT nod $ Node.miner
    storage nod config =
        hoistStorage (runNodeT nod) (Node.storage Rad.txEval Nakamoto.validateBlock config)
