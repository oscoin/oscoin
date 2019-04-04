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

import           Data.IP (IP)
import           Network.Socket (HostName, PortNumber)
import           Options.Applicative

data Options = Options
    { optHost          :: IP
    , optGossipPort    :: PortNumber
    , optApiPort       :: PortNumber
    , optDiscovery     :: P2P.Disco.Options
    , optNoEmptyBlocks :: Bool
    , optPaths         :: Paths
    , optEnvironment   :: Environment
    , optEkgHost       :: HostName
    , optEkgPort       :: PortNumber
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
    <*> switch
        ( long "no-empty-blocks"
       <> help "Do not generate empty blocks"
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
