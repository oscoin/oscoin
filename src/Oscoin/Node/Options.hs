module Oscoin.Node.Options
    ( Options(..)
    , nodeOptionsParser
    )
where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Configuration
                 ( ConfigPaths
                 , Environment
                 , Paths
                 , environmentParser
                 , pathsParser
                 )
import qualified Oscoin.P2P.Disco as P2P.Disco
import           Oscoin.Time (Duration, seconds)

import           Data.IP (IP)
import           Network.Socket (HostName, PortNumber)
import           Options.Applicative

data Options = Options
    { optHost           :: IP
    , optGossipPort     :: PortNumber
    , optApiPort        :: PortNumber
    , optDiscovery      :: P2P.Disco.Options P2P.Disco.OptNetwork
    , optBlockTimeLower :: Duration
    , optPaths          :: Paths
    , optEnvironment    :: Environment
    , optEkgHost        :: HostName
    , optEkgPort        :: PortNumber
    }

nodeOptionsParser :: ConfigPaths -> Parser Options
nodeOptionsParser cps = Options
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
    <*> option (map (* seconds) auto)
        ( long "block-time-lower"
       <> help "Lower bound on the block time. Applies only to empty blocks in \
               \the development environment, and is useful to avoid busy looping \
               \in an idle network."
       <> metavar "SECONDS"
       <> value 1
       <> showDefault
        )
    <*> pathsParser cps
    <*> environmentParser
    <*> option str
        ( long "ekg-host"
       <> help "Host name to bind to for the EKG server"
       <> value "127.0.0.1"
       <> showDefault
        )
    <*> option auto
        ( long "ekg-port"
       <> help "Port number to bind to for the EKG server"
       <> value 8090
       <> showDefault
        )
