{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Node.Options
    ( Options(..)

    , nodeOptionsParser
    , nodeOptionsOpts
    , renderNodeOptionsOpts
    )
where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Configuration
                 ( ConfigPaths
                 , ConsensusOptions
                 , Paths
                 , consensusOpts
                 , consensusParser
                 , pathsOpts
                 , pathsParser
                 )
import           Oscoin.Crypto.Blockchain.Block (Beneficiary)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey as Crypto
import           Oscoin.P2P.Disco (discoOpts, discoParser)
import qualified Oscoin.P2P.Disco as P2P.Disco
import qualified Oscoin.Telemetry.Options as Telemetry

import           Data.IP (IP)
import qualified Data.Text as T
import qualified Formatting as F
import           Network.Socket (HostName, PortNumber)
import           Options.Applicative
import           System.Console.Option

data Options crypto network = Options
    { optHost               :: IP
    , optGossipPort         :: PortNumber
    , optApiPort            :: PortNumber
    , optDiscovery          :: P2P.Disco.Options crypto network
    , optPaths              :: Paths
    , optConsensus          :: ConsensusOptions
    , optMetricsHost        :: Maybe HostName
    , optMetricsPort        :: Maybe PortNumber
    , optEkgHost            :: Maybe HostName
    , optEkgPort            :: Maybe PortNumber
    , optAllowEphemeralKeys :: Bool
    , optBeneficiary        :: Beneficiary crypto
    , optTelemetry          :: Telemetry.Options
    } deriving (Generic)

deriving instance (Eq (Crypto.PublicKey c),   Eq   (Beneficiary c), Eq   n) => Eq   (Options c n)
deriving instance (Show (Crypto.PublicKey c), Show (Beneficiary c), Show n) => Show (Options c n)

nodeOptionsParser
    :: (Crypto.HasHashing c)
    => ConfigPaths -> Parser (Options c P2P.Disco.OptNetwork)
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
    <*> discoParser
    <*> pathsParser cps
    <*> consensusParser
    <*> optional
        ( option str
          ( long "metrics-host"
         <> help "Host name to bind to for the prometheus metrics endpoint"
          )
        )
    <*> optional
        ( option auto
          ( long "metrics-port"
         <> help "Port number to bind to for the prometheus metrics endpoint"
          )
        )
    <*> optional
        ( option str
          ( long "ekg-host"
         <> help "Host name to bind to for the EKG server"
          )
        )
    <*> optional
        ( option auto
          ( long "ekg-port"
         <> help "Port number to bind to for the EKG server"
          )
        )
    <*> switch
        ( long "allow-ephemeral-keys"
       <> help "Create a fresh keypair if none could be found"
        )
    <*> option (maybeReader readBeneficiary)
        ( long "beneficiary"
       <> help "Beneficiary account id for block rewards. Hex encoded with leading 0x"
        )
    <*> Telemetry.telemetryOptionsParser
  where
    readBeneficiary = Crypto.parseShortHash . T.pack

nodeOptionsOpts
    :: Show (Crypto.ShortHash c)
    => P2P.Disco.CanRenderNetwork n
    => Options c n -> [Opt Text]
nodeOptionsOpts
    (Options
        optHost
        optGossipPort
        optApiPort
        optDiscovery
        optPaths
        optConsensus
        optMetricsHost
        optMetricsPort
        optEkgHost
        optEkgPort
        optAllowEphemeralKeys
        optBeneficiary
        optTelemetry) = concat
    [ pure . Opt "host"        $ show optHost
    , pure . Opt "gossip-port" $ show optGossipPort
    , pure . Opt "api-port"    $ show optApiPort
    , discoOpts optDiscovery
    , pathsOpts optPaths
    , consensusOpts optConsensus
    , maybe [] (pure . Opt "metrics-host" . toS)  optMetricsHost
    , maybe [] (pure . Opt "metrics-port" . show) optMetricsPort
    , maybe [] (pure . Opt "ekg-host"     . toS)  optEkgHost
    , maybe [] (pure . Opt "ekg-port"     . show) optEkgPort
    , bool  [] [Flag "allow-ephemeral-keys"] optAllowEphemeralKeys
    , pure . Opt "beneficiary"  $ show optBeneficiary
    , Telemetry.telemetryOptionsOpts optTelemetry
    ]

renderNodeOptionsOpts
    :: Show (Crypto.ShortHash c)
    => P2P.Disco.CanRenderNetwork n
    => Options c n -> [Text]
renderNodeOptionsOpts = map (F.sformat F.build) . nodeOptionsOpts
