module Main (main) where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.API.HTTP as HTTP
import           Oscoin.CLI.KeyStore (readKeyPair)
import qualified Oscoin.Consensus as Consensus
import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Consensus.Evaluator (EvalError, fromEvalError)
import qualified Oscoin.Consensus.Evaluator.Radicle as Rad
import           Oscoin.Crypto.Blockchain
                 (Blockchain, Difficulty, fromGenesis, (|>))
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock)
import           Oscoin.Crypto.Blockchain.Eval (buildBlockStrict)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (createTx)
import           Oscoin.Environment (Environment(Testing))
import           Oscoin.Logging (withStdLogger)
import qualified Oscoin.Logging as Log
import           Oscoin.Node (runNodeT, withNode)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import           Oscoin.P2P (mkNodeId, runGossipT, withGossip)
import qualified Oscoin.P2P as P2P
import           Oscoin.Storage (hoistStorage)
import qualified Oscoin.Storage.Block as BlockStore
import           Oscoin.Time


import qualified Control.Concurrent.Async as Async
import           Control.Monad.Except
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
           <> value 8477
           <> showDefault
            )
        <*> option str
            ( long "seeds"
           <> help "Path to YAML file describing gossip seed nodes"
           <> value "node/gossip-seeds.yaml"
            )
        <*> option str
            ( long "prelude"
           <> help "Path to radicle prelude"
           <> value "node/rad/prelude.rad"
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

    let consensus = Consensus.nakamotoConsensus difficulty

    keys     <- readKeyPair
    nid      <- pure (mkNodeId $ fst keys)
    mem      <- Mempool.newIO
    stree    <- STree.new Rad.pureEnv
    chain    <- either die pure =<< initialBlockchain prelude keys
    blkStore <- BlockStore.newIO $ BlockStore.initWithChain chain
    seeds'   <- Yaml.decodeFileThrow seeds

    withStdLogger  Log.defaultConfig { Log.cfgLevel = Log.Debug } $ \lgr ->
        withNode   (mkNodeConfig Testing lgr)
                   nid
                   mem
                   stree
                   blkStore
                   Rad.txEval
                   consensus                                      $ \nod ->
        withGossip lgr
                   keys
                   P2P.NodeAddr { P2P.nodeId   = nid
                                , P2P.nodeHost = host
                                , P2P.nodePort = gossipPort
                                }
                   seeds'
                   (storage nod)                                  $ \gos ->
            Async.runConcurrently $
                     Async.Concurrently (HTTP.run (fromIntegral apiPort) Testing nod)
                  <> Async.Concurrently (miner nod gos)
  where
    mkNodeConfig env lgr = Node.Config
        { Node.cfgEnv = env
        , Node.cfgLogger = lgr
        }

    miner nod gos = runGossipT gos . runNodeT nod $ Node.miner
    storage nod   = hoistStorage (runNodeT nod) Node.storage

-- | Creates a blockchain with two blocks: an empty gensis block and a
-- block containing a single Radicle transaction loaded from @path@ and
-- signed with @keypair@.
initialBlockchain
  :: FilePath
  -> Crypto.KeyPair
  -> IO (Either Text (Blockchain Rad.RadTx Rad.Env))
initialBlockchain path keypair = runExceptT $ do
    val <- ExceptT $ Rad.parseValue (T.pack path) <$> readFile path
    tx <- liftIO $ createTx keypair val
    now' <- now
    let genesis = emptyGenesisBlock epoch Rad.pureEnv
    let chain = fromGenesis genesis
    block <- withExceptT formatBuildError $ liftEither $
        buildBlockStrict Rad.txEval now' [tx] genesis
    pure $ block |> chain
  where
    formatBuildError :: (Rad.RadTx, EvalError) -> Text
    formatBuildError (_tx, evalErrors) =
        "Error applying initial transactions:\n" <> fromEvalError evalErrors
