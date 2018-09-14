module Main (main) where

import           Oscoin.Prelude

import           Oscoin.Crypto.PubKey (generateKeyPair)
import           Oscoin.Logging (Logger, withStdLogger)
import qualified Oscoin.Logging as Log
import           Oscoin.P2P (Endpoints(..), NodeAddr(..), mkNodeId, withP2P)
import qualified Oscoin.P2P as P2P
import           Oscoin.P2P.Discovery (Disco, knownPeers, withDisco)
import qualified Oscoin.P2P.Discovery.Multicast as MCast

import           Control.Concurrent (threadDelay)
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import           System.Exit (die)

import           Options.Generic

data Args = Args
    { tx         :: Text
    , listenIp   :: Text
    , listenPort :: Word16
    } deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
    args@Args{..} <- getRecord "oscoin transaction submitter"
    print (args :: Args)

    nid <- mkNodeId . fst <$> generateKeyPair -- TODO: read from disk
    let !ip = read listenIp

    withStdLogger Log.defaultConfig                   $ \lgr   ->
        withDisco (mkDisco lgr nid ip listenPort)     $ \disco ->
        withP2P (mkP2PConfig ip listenPort) lgr disco $ \p2p   -> do
            waitPeers lgr disco 10
            P2P.send p2p [P2P.TxMsg [tx]]
  where
    waitPeers :: Logger -> Disco IO -> Word8 -> IO ()
    waitPeers _ _ 0 = die "No peers discovered"
    waitPeers l d n = do
        Log.info l "Waiting for peers..."
        peers <- nonEmpty . Map.toList <$> knownPeers d
        case peers of
            Nothing -> do
                threadDelay (2 * 1000000)
                waitPeers l d (n - 1)
            _ -> pure ()

    mkP2PConfig ip port = P2P.defaultConfig
        { P2P.cfgBindIP   = ip
        , P2P.cfgBindPort = port
        }

    mkDisco lgr nid ip =
        MCast.mkDisco lgr . MCast.mkConfig nid . endpoints ip

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
