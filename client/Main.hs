module Main (main) where

import           Oscoin.P2P.Simple
import           Oscoin.Prelude

import qualified Data.IP as IP
import qualified Network.Socket as NS
import           GHC.Generics (Generic)

import Options.Generic

data Args = Args { listenIp :: Text, listenPort :: Int }
    deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
    args <- getRecord "oscoin cli"
    print (args :: Args)

    let ip = read (listenIp args) :: IP.IPv4
        host = IP.toHostAddress ip
        port = fromIntegral $ listenPort args :: NS.PortNumber
        addr = NS.SockAddrInet port host
     in runProto addr port
