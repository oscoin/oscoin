module Main (main) where

import           Oscoin.Prelude hiding (option)

import           Oscoin.API.HTTP (withAPI)
import qualified Oscoin.API.HTTP as HTTP
import           Oscoin.CLI.KeyStore (readKeyPair)
import qualified Oscoin.Consensus as Consensus
import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Consensus.BlockStore.Class (chainState)
import           Oscoin.Consensus.Class (updateM)
import           Oscoin.Consensus.Evaluator (fromEvalError)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain (Difficulty)
import           Oscoin.Crypto.Blockchain.Block (genesisBlock)
import           Oscoin.Data.Tx (createTx)
import           Oscoin.Environment (Environment(Testing))
import           Oscoin.Logging (withStdLogger)
import qualified Oscoin.Logging as Log
import           Oscoin.Node (withNode)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import           Oscoin.P2P (mkNodeId, runGossip)
import qualified Oscoin.P2P as P2P
import qualified Oscoin.Storage as Storage
import qualified Oscoin.Storage.Block as BlockStore

import qualified Oscoin.Consensus.Evaluator.Radicle as Rad

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import           Data.Default (def)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           GHC.Generics (Generic)
import           Network.Socket (HostName, PortNumber)

import           Options.Applicative

data Args = Args
    { host       :: HostName
    , gossipPort :: PortNumber
    , apiPort    :: PortNumber
    , seeds      :: FilePath
    , prelude    :: FilePath
    , difficulty :: Maybe Difficulty
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
           <> value 8080
           <> showDefault
            )
        <*> option str
            ( long "seeds"
           <> help "Path to YAML file describing gossip seed nodes"
            )
        <*> option str
            ( long "prelude"
           <> help "Path to radicle prelude"
            )
        <*> optional
            ( option auto
              ( long "difficulty"
             <> help "Mining difficulty"
              )
            )

main :: IO ()
main = do
    Args{..} <- execParser args

    let consensus  = Consensus.nakamotoConsensus
                   $ fromMaybe Nakamoto.easyDifficulty difficulty
    let eval       = Node.nodeEval

    kp       <- readKeyPair
    nid      <- pure (mkNodeId $ fst kp)
    mem      <- Mempool.newIO
    stree    <- STree.new def
    gen      <- either die pure =<< genesisFromPath eval prelude kp
    blkStore <- BlockStore.newIO $ genesisBlockStore gen
    seeds'   <- Yaml.decodeFileThrow seeds

    withStdLogger Log.defaultConfig { Log.cfgLevel = Log.Debug }    $ \lgr ->
        withNode  (mkNodeConfig Testing lgr) nid mem stree blkStore $ \nod ->
        withAPI   Testing                                           $ \api ->
        runGossip lgr kp
                  P2P.NodeAddr { P2P.nodeId   = nid
                               , P2P.nodeHost = host
                               , P2P.nodePort = gossipPort
                               }
                  seeds'
                  (storage consensus eval nod)                      $ \gos ->
            Async.runConcurrently $
                     Async.Concurrently (HTTP.run api (fromIntegral apiPort) nod)
                  <> Async.Concurrently (miner consensus eval gos nod)
  where
    mkNodeConfig env lgr = Node.Config
        { Node.cfgEnv = env
        , Node.cfgLogger = lgr
        }

    genesisFromPath eval path kp = runExceptT $ do
        val <- ExceptT $ Rad.parseValue (T.pack path) <$> readFile path
        withExceptT (T.unlines . map fromEvalError) . ExceptT $ do
            tx <- liftIO $ createTx kp val
            pure $ genesisBlock def eval 0 [tx]

    miner consensus eval gos nod =
        Node.runNodeT nod $ Node.miner consensus eval gos

    storage consensus eval nod =
        P2P.storageCallbacks applyBlock applyTx lookupBlock lookupTx
      where
        applyBlock =
            Node.runNodeT nod . (Storage.applyBlock eval >=> updateChainState)
        applyTx    =
            Node.runNodeT nod . (Storage.applyTx >=> updateChainState)

        updateChainState applyRes = do
            when (applyRes == Storage.Applied) $
                updateM =<< chainState (Consensus.cScore consensus)
            pure applyRes

        lookupBlock = Node.runNodeT nod . Storage.lookupBlock
        lookupTx    = Node.runNodeT nod . Storage.lookupTx
