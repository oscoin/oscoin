module Main (main) where

import           Oscoin.Consensus.Simple
import           Oscoin.P2P.Discovery
import           Oscoin.Prelude

import qualified Network.Multicast as NMC
import qualified Network.Socket.ByteString as NSB
import qualified Data.Binary as Binary (encode)
import qualified Data.ByteString.Lazy as LBS

import Options.Generic

data Args = Args { tx :: Text }
    deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
    args <- getRecord "oscoin transaction submitter"
    print (args :: Args)

    let serialized = LBS.toStrict $ Binary.encode $ ClientTx (tx args)

    sock <- NMC.multicastReceiver mcastDiscoGroup mcastDiscoPort
    peer <- discoverPeer sock

    NSB.sendAllTo sock serialized peer
