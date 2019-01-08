module Main (main) where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.API.HTTP as HTTP
import           Oscoin.CLI.KeyStore (readKeyPair)
import qualified Oscoin.Consensus as Consensus
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain (fromGenesis)
import           Oscoin.Crypto.Blockchain.Block (Block)
import           Oscoin.Crypto.Blockchain.Eval (evalBlock, fromEvalError)
import           Oscoin.Data.RadicleTx (RadTx)
import qualified Oscoin.Data.RadicleTx as Rad (pureEnv, txEval)
import           Oscoin.Environment (Environment(Development))
import           Oscoin.Logging (withStdLogger)
import qualified Oscoin.Logging as Log
import           Oscoin.Node (runNodeT, withNode)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import           Oscoin.P2P (mkNodeId, runGossipT, withGossip)
import qualified Oscoin.P2P as P2P
import qualified Oscoin.P2P.Handshake as Handshake
import           Oscoin.ProtocolConfig (getProtocolConfig)
import           Oscoin.Storage (hoistStorage)
import qualified Oscoin.Storage.Block as BlockStore
import qualified Oscoin.Storage.Block.STM as BlockStore
import qualified Oscoin.Storage.State as StateStore

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


main :: IO ()
main = do
    Args{..} <- execParser args

    let consensus = Consensus.nakamotoConsensus
    let env       = Development

    keys        <- readKeyPair
    nid         <- pure (mkNodeId $ fst keys)
    mem         <- Mempool.newIO
    gen         <- Yaml.decodeFileThrow genesis :: IO (Block RadTx Nakamoto.PoW)
    genState    <- either (die . fromEvalError) pure (evalBlock Rad.txEval Rad.pureEnv gen)
    stStore     <- StateStore.fromStateM genState
    blkStore    <- BlockStore.newIO $ BlockStore.initWithChain (fromGenesis gen)
    seeds'      <- Yaml.decodeFileThrow seeds
    protoConfig <- getProtocolConfig env

    withStdLogger  Log.defaultConfig { Log.cfgLevel = Log.Debug } $ \lgr ->
        withNode   (mkNodeConfig env lgr noEmptyBlocks protoConfig)
                   nid
                   mem
                   stStore
                   blkStore
                   Rad.txEval
                   consensus                                      $ \nod ->
        withGossip lgr
                   P2P.NodeAddr { P2P.nodeId   = nid
                                , P2P.nodeHost = host
                                , P2P.nodePort = gossipPort
                                }
                   seeds'
                   (storage nod protoConfig)
                   (Handshake.simpleHandshake keys)               $ \gos ->
            Async.runConcurrently $
                     Async.Concurrently (HTTP.run (fromIntegral apiPort) Development nod)
                  <> Async.Concurrently (miner nod gos)
  where
    mkNodeConfig env lgr neb protocolConfig = Node.Config
        { Node.cfgEnv = env
        , Node.cfgLogger = lgr
        , Node.cfgNoEmptyBlocks = neb
        , Node.cfgProtocolConfig = protocolConfig
        }

    miner nod gos = runGossipT gos . runNodeT nod $ Node.miner
    storage nod protocolConfig =
        hoistStorage (runNodeT nod) (Node.storage Rad.txEval Nakamoto.validateBlock protocolConfig)
