module Main (main) where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.API.HTTP as HTTP
import           Oscoin.CLI.KeyStore (readKeyPair)
import           Oscoin.CLI.Parser (environmentParser, keyPathParser)
import qualified Oscoin.Consensus as Consensus
import qualified Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Blockchain.Block (Block, Sealed)
import           Oscoin.Crypto.Blockchain.Eval (evalBlock, fromEvalError)
import           Oscoin.Data.RadicleTx (RadTx)
import qualified Oscoin.Data.RadicleTx as Rad (pureEnv, txEval)
import           Oscoin.Environment (Environment(..), toText)
import           Oscoin.Node (runNodeT, withNode)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.P2P (mkNodeId, runGossipT, withGossip)
import qualified Oscoin.P2P as P2P
import qualified Oscoin.P2P.Disco as P2P.Disco
import qualified Oscoin.P2P.Disco.MDns as MDns
import qualified Oscoin.P2P.Handshake as Handshake
import           Oscoin.Protocol (runProtocol)
import           Oscoin.Storage (hoistStorage)
import qualified Oscoin.Storage.Block.SQLite as BlockStore.SQLite
import qualified Oscoin.Storage.State as StateStore
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (withStdLogger)
import qualified Oscoin.Telemetry.Logging as Log
import           Oscoin.Telemetry.Metrics

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Managed (managed, runManaged)
import           Data.IP (IP)
import qualified Data.Set as Set
import qualified Data.Yaml as Yaml
import           Network.Socket (HostName, PortNumber)

import           Options.Applicative

data Args = Args
    { host           :: IP
    , gossipPort     :: PortNumber
    , apiPort        :: PortNumber
    , discoOpts      :: P2P.Disco.Options
    , genesis        :: FilePath
    , noEmptyBlocks  :: Bool
    , keysPath       :: Maybe FilePath
    , environment    :: Environment
    , ekgHost        :: HostName
    , ekgPort        :: PortNumber
    , blockStorePath :: FilePath
    }

args :: ParserInfo Args
args = info (helper <*> parser) $ progDesc "Oscoin Node"
  where
    parser = Args
        <$> option auto
            ( short 'h'
           <> long "host"
           <> help "IP address to bind to (both API and gossip)"
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
        <*> P2P.Disco.discoParser
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
        <*> environmentParser
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
        <*> blockStorePathParser

    blockStorePathParser :: Parser FilePath
    blockStorePathParser = option str (
                                 long "blockstore"
                              <> help "The path to the database storing the blocks. "
                              <> metavar "FILEPATH"
                              <> value "blockstore.db"
                              <> showDefault
                              )


type GenesisBlock =
    Block Crypto (RadTx Crypto) (Sealed Crypto Nakamoto.PoW)

main :: IO ()
main = do
    Args{..} <- execParser args

    let consensus = case environment of
                      Production  -> Consensus.nakamotoConsensus
                      Development -> Consensus.nakamotoConsensusLenient
                      Testing     -> Consensus.nakamotoConsensusLenient

    keys         <- runReaderT readKeyPair keysPath
    nid          <- pure (mkNodeId $ fst keys)
    mem          <- Mempool.newIO
    gen          <- Yaml.decodeFileThrow genesis :: IO GenesisBlock
    genState     <- either (die . fromEvalError) pure $
                        evalBlock Rad.txEval Rad.pureEnv gen
    stStore      <- StateStore.fromStateM genState
    metricsStore <- newMetricsStore $ labelsFromList [("env", toText environment)]
    let consensusConfig = Consensus.getConfig environment

    res :: Either SomeException () <-
          try
        $ withStdLogger (Log.configForEnvironment environment) $ \lgr ->
          Log.withExceptionLogged lgr
        . runManaged
        $ do
            let telemetry = Telemetry.newTelemetryStore lgr metricsStore

            blkStore@(blkStoreReader,_) <- managed $
                BlockStore.SQLite.withBlockStore blockStorePath gen

            proto <- managed $
                runProtocol (Consensus.cValidate consensus)
                            Nakamoto.blockScore
                            telemetry
                            blkStore
                            consensusConfig

            node <- managed $
                let config = mkNodeConfig environment
                                          telemetry
                                          noEmptyBlocks
                                          consensusConfig
                 in withNode config
                             nid
                             mem
                             stStore
                             blkStoreReader
                             proto
                             Rad.txEval
                             consensus

            disco <- managed $
                let
                    instr :: HasCallStack => P2P.Disco.DiscoEvent -> IO ()
                    instr = Telemetry.emit telemetry . Telemetry.DiscoEvent
                 in
                    P2P.Disco.withDisco instr discoOpts $ Set.fromList
                        [ MDns.Service "gossip" MDns.TCP host gossipPort
                        , MDns.Service "http"   MDns.TCP host apiPort
                        ]

            seeds  <- liftIO disco
            gossip <- managed $
                withGossip telemetry
                           P2P.NodeAddr { P2P.nodeId   = pure nid
                                        , P2P.nodeHost = P2P.numericHost host
                                        , P2P.nodePort = gossipPort
                                        }
                           (Set.map (Nothing,) seeds)
                           (storage node)
                           (Handshake.simpleHandshake keys)

            liftIO $ do
                forkEkgServer metricsStore ekgHost ekgPort
                Async.runConcurrently $
                         Async.Concurrently (HTTP.run (fromIntegral apiPort) node)
                      <> Async.Concurrently (miner node gossip)

    either (const exitFailure) (const exitSuccess) res
  where
    mkNodeConfig env telemetryHandle neb config = Node.Config
        { Node.cfgEnv = env
        , Node.cfgTelemetry = telemetryHandle
        , Node.cfgNoEmptyBlocks = neb
        , Node.cfgConsensusConfig = config
        }

    miner nod gos = runGossipT gos . runNodeT nod $ Node.miner
    storage nod =
        hoistStorage (runNodeT nod) (Node.storage Nakamoto.validateBasic)
