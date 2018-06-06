module Main (main) where

import           Oscoin.Consensus.Simple
import           Oscoin.P2P.Discovery
import qualified Oscoin.P2P.Discovery.Multicast as MCast
import           Oscoin.Prelude

import           Control.Concurrent (threadDelay)
import qualified Data.Binary as Binary (encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Set as Set
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import           System.Exit (die)

import           Options.Generic

data Args = Args { tx :: Text }
    deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
    args <- getRecord "oscoin transaction submitter"
    print (args :: Args)

    sock  <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
    peers <- discoverPeers
    case peers of
        Nothing -> die "No peers discovered"
        Just ps -> do
            let serialized = LBS.toChunks $ Binary.encode $ ClientTx (tx args)
            for_ ps $ NSB.sendManyTo sock serialized
  where
    discoverPeers :: IO (Maybe (NonEmpty NS.SockAddr))
    discoverPeers =
        withDisco (MCast.mkDisco (MCast.mkConfig 0)) $ \disco ->
            knownPeers' disco (5 :: Word8)

    knownPeers' _     0 = pure Nothing
    knownPeers' disco n = do
        peers <- nonEmpty . Set.toList <$> knownPeers disco
        case peers of
            Nothing -> do
                threadDelay (2 * 1000000)
                knownPeers' disco (n - 1)
            peers'  -> pure peers'
