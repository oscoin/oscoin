module Main (main) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Consensus.Nakamoto (evalNakamotoT, defaultNakamotoEnv, NakamotoEnv(..), easyDifficulty)
import           Oscoin.Crypto.Blockchain (Difficulty)
import           Oscoin.Crypto.Blockchain.Block (genesisBlock)
import           Oscoin.Crypto.PubKey (generateKeyPair, publicKeyHash)
import           Oscoin.Data.Tx (createTx)
import           Oscoin.Environment (Environment(Testing))
import qualified Oscoin.API.HTTP as HTTP
import           Oscoin.API.HTTP (withAPI)
import           Oscoin.Logging (withStdLogger)
import qualified Oscoin.Logging as Log
import qualified Oscoin.Node as Node
import           Oscoin.Node (withNode, nodeEval)
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import           Oscoin.P2P (Endpoints(..), NodeAddr(..), NodeId(..), withP2P)
import qualified Oscoin.P2P as P2P
import           Oscoin.P2P.Discovery (withDisco, toKnownPeers)
import qualified Oscoin.P2P.Discovery.Multicast as MCast
import qualified Oscoin.P2P.Discovery.Static as Static
import qualified Oscoin.Storage.Block as BlockStore

import qualified Oscoin.Consensus.Evaluator.Radicle as Rad

import qualified Control.Concurrent.Async as Async
import qualified Data.Yaml as Yaml
import           GHC.Generics (Generic)
import           System.Random (newStdGen)
import qualified Data.Text as T

import           Options.Generic

data Args = Args
    { listen        :: Text
    , seed          :: [FilePath]
    , prelude       :: FilePath
    , difficulty    :: Maybe Difficulty
    } deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
    Args{..} <- getRecord "oscoin"
    let P2P.NodeAddr{..} = read listen
    let dif = maybe easyDifficulty identity difficulty

    kp  <- generateKeyPair
    nid <- pure (NodeId . publicKeyHash . fst $ kp) -- TODO: read from disk
    rng <- newStdGen
    mem <- Mempool.newIO
    str <- STree.new
    gen <- genesisFromPath prelude kp
    blk <- BlockStore.newIO $ genesisBlockStore gen
    sds <- traverse Yaml.decodeFileThrow seed :: IO [P2P.Seed]

    withStdLogger Log.defaultConfig { Log.cfgLevel = Log.Debug }        $ \lgr ->
        withNode  (mkNodeConfig Testing lgr) nid mem str blk            $ \nod ->
        withAPI   Testing                                               $ \api ->
        withDisco (mkDisco lgr sds nid addrIP addrPort)                 $ \dis ->
        withP2P   (mkP2PConfig addrIP addrPort) lgr dis                 $ \p2p ->
            let run = Node.runEffects p2p nod (evalNakamotoT env rng)
                env = defaultNakamotoEnv
                    { nakEval = nodeEval
                    , nakLogger = lgr
                    , nakDifficulty = dif
                    }
             in do
                 void $ Async.async $ HTTP.run api 8080 nod -- TODO(cloudhead): Eventually we should terminate gracefully.
                 Async.race_ (run . forever $ Node.step)
                             (run . forever $ Node.tick)
  where
    mkNodeConfig env lgr = Node.Config
        { Node.cfgEnv = env
        , Node.cfgLogger = lgr
        }
    mkP2PConfig ip port = P2P.defaultConfig
        { P2P.cfgBindIP   = ip
        , P2P.cfgBindPort = port
        }

    genesisFromPath path kp = do
        result <- Rad.parseValue (T.pack path) <$> readFile path
        case result of
            Left err -> error $
                "Main.hs: error reading prelude: " ++ T.unpack err
            Right val -> do
                tx <- createTx kp val
                case genesisBlock def nodeEval 0 [tx] of
                    Left errs ->
                        error $ "Main.hs: error evaluating prelude:\n" ++ unlines (map show errs)
                    Right blk ->
                        pure blk

    mkDisco lgr [] nid ip prt = MCast.mkDisco lgr . MCast.mkConfig nid $ endpoints ip prt
    mkDisco _   ss _   _  _   = pure . Static.mkDisco . toKnownPeers $ ss

    endpoints ip port = Endpoints
        { apiEndpoint = NodeAddr
            { addrIP   = ip
            , addrPort = 8080
            }
        , p2pEndpoint = NodeAddr
            { addrIP   = ip
            , addrPort = port
            }
        }
