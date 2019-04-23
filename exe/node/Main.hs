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
import qualified Oscoin.Data.OscoinTx as OscoinTx
import           Oscoin.Data.Tx
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
import qualified Oscoin.Storage.Ledger as Ledger
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Telemetry.Logging (withStdLogger)
import qualified Oscoin.Telemetry.Logging as Log
import           Oscoin.Telemetry.Metrics
import           Oscoin.Telemetry.Middleware (telemetryMiddleware)

import           Control.Monad.Managed (managed, runManaged)
import qualified Data.Set as Set
import           Data.String (fromString)
import qualified Data.Yaml as Yaml
import           Network.HTTP.Types.Status (notFound404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import           Options.Applicative

type GenesisBlock =
    Block Crypto (Tx Crypto) (Sealed Crypto Nakamoto.PoW)

main :: IO ()
main = do
    cfgPaths <- getConfigPaths
    Node.Options{..} <- execParser $
        info (helper <*> Node.nodeOptionsParser cfgPaths)
             (progDesc "Oscoin Node")

    keys         <- runReaderT readKeyPair (Just $ keysDir optPaths)
    nid          <- pure (mkNodeId $ fst keys)
    mem          <- Mempool.newIO
    gen          <- Yaml.decodeFileThrow (genesisPath optPaths) :: IO GenesisBlock
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

            let probe   = Telemetry.telemetryProbe telemetry
                        & Telemetry.hoistProbe liftIO
            let consensus =
                    case optEnvironment of
                        Production  ->
                            Consensus.nakamotoConsensus (Telemetry.probed probe)
                        Development ->
                            Consensus.nakamotoConsensusLenient (Telemetry.probed probe) optBlockTimeLower


            blkStore <- managed $
                BlockStore.SQLite.withBlockStore (blockstorePath optPaths) gen
            -- FIXME(adn) Replace with a proper evaluator & state once we switch to
            -- the OscoinTx type.
            let dummyEval _ s = Right (OscoinTx.TxOutput, s)
            ledger <- liftIO $ Ledger.newFromBlockStoreIO dummyEval (fst blkStore) mempty

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
                             ledger
                             proto
                             consensus

            disco <- managed $
                let
                    instr :: HasCallStack => P2P.Disco.DiscoEvent -> IO ()
                    instr = Telemetry.emit telemetry . Telemetry.DiscoEvent
                 in
                    P2P.Disco.withDisco instr optDiscovery' optGossipPort $
                        Set.fromList
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
                <> (Concurrently $
                        maybeRunMetrics telemetry optMetricsHost optMetricsPort)
                <> (Concurrently $
                        maybeRunEkg metricsStore optEkgHost optEkgPort)

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

    maybeRunEkg store (Just host) (Just port) = do
        tid <- forkEkgServer store host port
        withException (threadDelay maxBound) $ \(e :: SomeAsyncException) ->
            throwTo tid e
    maybeRunEkg _ _ _ = pure ()

    maybeRunMetrics store (Just host) (Just port) =
        let
            settings =
                  Warp.setHost (fromString host)
                . Warp.setPort (fromIntegral port)
                . Warp.setOnException (\_ ex ->
                    when (Warp.defaultShouldDisplayException ex) $
                        throwIO ex)
                $ Warp.defaultSettings

            app =
                telemetryMiddleware store $ \_ respond ->
                    respond $ Wai.responseLBS notFound404 mempty mempty
         in
            Warp.runSettings settings app
    maybeRunMetrics _ _ _ = pure ()
