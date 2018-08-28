module Main (main) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Consensus.Nakamoto (evalNakamotoT, defaultNakamotoEnv, nakEval, nakLogger)
import           Oscoin.Consensus.Evaluator (radicleEval)
import           Oscoin.Crypto.Blockchain.Block (emptyGenesisBlock)
import           Oscoin.Crypto.PubKey (generateKeyPair, publicKeyHash)
import           Oscoin.Data.Tx (Tx)
import           Oscoin.Environment (Environment(Testing))
import qualified Oscoin.HTTP as HTTP
import           Oscoin.HTTP (withAPI)
import           Oscoin.Logging (withStdLogger)
import qualified Oscoin.Logging as Log
import qualified Oscoin.Node as Node
import           Oscoin.Node (withNode)
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import           Oscoin.P2P (Endpoints(..), NodeAddr(..), NodeId(..), withP2P)
import qualified Oscoin.P2P as P2P
import           Oscoin.P2P.Discovery (withDisco, toKnownPeers)
import qualified Oscoin.P2P.Discovery.Multicast as MCast
import qualified Oscoin.P2P.Discovery.Static as Static
import qualified Oscoin.Storage.Block as BlockStore

import qualified Control.Concurrent.Async as Async
import           Data.Proxy (Proxy(..))
import qualified Data.Yaml as Yaml
import           GHC.Generics (Generic)
import           System.Random (newStdGen)

import           Options.Generic

data Args = Args { listenIp :: Text, listenPort :: Word16, seed :: [FilePath] }
    deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
    args@Args{..} <- getRecord "oscoin cli"
    print (args :: Args)

    nid <- NodeId . publicKeyHash . fst <$> generateKeyPair -- TODO: read from disk
    rng <- newStdGen
    mem <- Mempool.newIO
    str <- STree.connect
    blk <- BlockStore.newIO $ genesisBlockStore $ emptyGenesisBlock 0
    sds <- traverse Yaml.decodeFileThrow seed :: IO [P2P.Seed]

    let !ip = read listenIp

    withStdLogger Log.defaultConfig { Log.cfgLevel = Log.Debug }        $ \lgr ->
        withNode  (Node.Config "xyz" [] Testing lgr) nid mem str blk    $ \nod ->
        withAPI   Testing                                               $ \api ->
        withDisco (mkDisco lgr sds nid ip listenPort)                   $ \dis ->
        withP2P   (mkP2PConfig ip listenPort) lgr dis                   $ \p2p ->
            let run = Node.runEffects p2p nod (evalNakamotoT env rng)
                env = defaultNakamotoEnv { nakEval = radicleEval, nakLogger = lgr }
             in do
                 void $ Async.async $ HTTP.run api 8080 nod -- TODO(cloudhead): Eventually we should terminate gracefully.
                 Async.race_ (run . forever $ Node.step)
                             (run . forever $ Node.tick)
  where
    mkP2PConfig ip port = P2P.defaultConfig
        { P2P.cfgBindIP   = ip
        , P2P.cfgBindPort = port
        }

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
