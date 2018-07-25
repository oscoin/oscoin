module Main (main) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Consensus.Nakamoto (evalNakamotoT, defaultNakamotoEnv)
import           Oscoin.Crypto.Blockchain.Block (genesisBlock)
import           Oscoin.Crypto.PubKey (generateKeyPair)
import           Oscoin.Environment (Environment(Testing))
import           Oscoin.Logging (withStdLogger)
import qualified Oscoin.Logging as Log
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import           Oscoin.P2P (Endpoints(..), NodeAddr(..), NodeId(..), withP2P)
import qualified Oscoin.P2P as P2P
import           Oscoin.P2P.Discovery (withDisco)
import qualified Oscoin.P2P.Discovery.Multicast as MCast
import qualified Oscoin.Storage.Block as BlockStore

import qualified Control.Concurrent.Async as Async
import           Data.Proxy (Proxy(..))
import           GHC.Generics (Generic)
import           System.Random (newStdGen)

import           Options.Generic

data Args = Args { listenIp :: Text, listenPort :: Word16 }
    deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
    args@Args{..} <- getRecord "oscoin cli"
    print (args :: Args)

    nid <- NodeId . fst <$> generateKeyPair -- TODO: read from disk
    rng <- newStdGen
    mem <- Mempool.new
    str <- STree.connect
    blk <- BlockStore.new $ genesisBlockStore (genesisBlock 0 [])
    nod <- Node.open (Node.Config "xyz" [] Testing []) nid mem str blk

    let !ip = read listenIp

    withStdLogger Log.defaultConfig                   $ \lgr ->
        withDisco (mkDisco lgr nid ip listenPort)     $ \dis ->
        withP2P   (mkP2PConfig ip listenPort) lgr dis $ \p2p ->
            let run = Node.runEffects p2p nod (evalNakamotoT defaultNakamotoEnv rng)
             in Async.race_ (run . forever $ Node.step (Proxy @Text))
                            (run . forever $ Node.tick (Proxy @Text))
  where
    mkP2PConfig ip port = P2P.defaultConfig
        { P2P.cfgBindIP   = ip
        , P2P.cfgBindPort = port
        }

    mkDisco lgr nid ip = MCast.mkDisco lgr . MCast.mkConfig nid . endpoints ip

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
