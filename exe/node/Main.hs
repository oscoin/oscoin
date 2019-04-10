module Main (main) where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.API.HTTP as HTTP
import           Oscoin.CLI.KeyStore (readKeyPair)
import           Oscoin.Configuration
import qualified Oscoin.Consensus as Consensus
import qualified Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Blockchain.Block (Block, Sealed)
import           Oscoin.Crypto.Blockchain.Eval (evalBlock)
import           Oscoin.Data.RadicleTx (RadTx)
import qualified Oscoin.Data.RadicleTx as Rad (pureEnv, txEval)
import           Oscoin.Node (runNodeT, withNode)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.Node.Options (Options(..))
import qualified Oscoin.Node.Options as Node
import           Oscoin.P2P (mkNodeId, runGossipT, withGossip)
import qualified Oscoin.P2P as P2P
import qualified Oscoin.P2P.Disco as P2P.Disco
import qualified Oscoin.P2P.Disco.MDns as MDns
import qualified Oscoin.P2P.Handshake as Handshake
import           Oscoin.Protocol (runProtocol)
import           Oscoin.Storage (hoistStorage)
import qualified Oscoin.Storage.Block.SQLite as BlockStore.SQLite
import           Oscoin.Storage.HashStore
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (withStdLogger)
import qualified Oscoin.Telemetry.Logging as Log
import           Oscoin.Telemetry.Metrics

import           Control.Monad.Managed (managed, runManaged)
import qualified Data.Set as Set
import qualified Data.Yaml as Yaml

import           Options.Applicative

type GenesisBlock =
    Block Crypto (RadTx Crypto) (Sealed Crypto Nakamoto.PoW)

main :: IO ()
main = do
    cfgPaths <- getConfigPaths
    Node.Options{..} <- execParser $
        info (helper <*> Node.nodeOptionsParser cfgPaths)
             (progDesc "Oscoin Node")

    let consensus =
            case optEnvironment of
                Production  -> Consensus.nakamotoConsensus
                Development -> Consensus.nakamotoConsensusLenient optBlockTimeLower

    keys         <- runReaderT readKeyPair (Just $ keysDir optPaths)
    nid          <- pure (mkNodeId $ fst keys)
    mem          <- Mempool.newIO
    gen          <- Yaml.decodeFileThrow (genesisPath optPaths) :: IO GenesisBlock
    let (genState, _receipts) = evalBlock Rad.txEval Rad.pureEnv gen
    stStore      <- newHashStoreIO
    storeHashContent stStore genState
    metricsStore <-
        newMetricsStore $
            labelsFromList [("env", renderEnvironment optEnvironment)]
    let consensusConfig = Consensus.configForEnvironment optEnvironment

    optDiscovery' <-
        either (die . toS) pure =<< P2P.Disco.evalOptions optDiscovery

    res :: Either SomeException () <-
          try
        $ withStdLogger (Log.configForEnvironment optEnvironment) $ \lgr ->
          Log.withExceptionLogged lgr
        . runManaged
        $ do
            let telemetry = Telemetry.newTelemetryStore lgr metricsStore

            blkStore@(blkStoreReader,_) <- managed $
                BlockStore.SQLite.withBlockStore (blockstorePath optPaths) gen

            proto <- managed $
                runProtocol (Consensus.cValidate consensus)
                            Nakamoto.blockScore
                            telemetry
                            blkStore
                            consensusConfig

            node <- managed $
                let
                    config =
                        mkNodeConfig optEnvironment
                                     (P2P.Disco.optNetwork optDiscovery')
                                     telemetry
                                     consensusConfig
                 in
                    withNode config
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
                    P2P.Disco.withDisco instr optDiscovery' $ Set.fromList
                        [ MDns.Service "gossip" MDns.TCP optHost optGossipPort
                        , MDns.Service "http"   MDns.TCP optHost optApiPort
                        ]

            seeds  <- liftIO disco
            gossip <- managed $
                withGossip telemetry
                           P2P.NodeAddr { P2P.nodeId   = pure nid
                                        , P2P.nodeHost = P2P.numericHost optHost
                                        , P2P.nodePort = optGossipPort
                                        }
                           (Set.map (Nothing,) seeds)
                           (storage node)
                           (Handshake.secureHandshake
                               keys (P2P.Disco.optNetwork optDiscovery'))

            liftIO . runConcurrently $
                   Concurrently (HTTP.run (fromIntegral optApiPort) node)
                <> Concurrently (miner node gossip)
                <> Concurrently (runEkg metricsStore optEkgHost optEkgPort)

    either (const exitFailure) (const exitSuccess) res
  where
    mkNodeConfig env net metrics consensusConfig
        = Node.Config
            { Node.cfgGlobalConfig    = Node.GlobalConfig
                { Node.globalEnv             = env
                , Node.globalLogicalNetwork  = P2P.fromPhysicalNetwork net
                , Node.globalPhysicalNetwork = net
                }
            , Node.cfgTelemetry       = metrics
            , Node.cfgConsensusConfig = consensusConfig
            }

    miner nod gos = runGossipT gos . runNodeT nod $ Node.miner
    storage nod =
        hoistStorage (runNodeT nod) (Node.storage Nakamoto.validateBasic)

    runEkg store host port = do
        tid <- forkEkgServer store host port
        withException (threadDelay maxBound) $ \(e :: SomeAsyncException) ->
            throwTo tid e
