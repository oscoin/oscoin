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
                 , Environment
                 , Paths
                 , environmentOpts
                 , environmentParser
                 , pathsOpts
                 , pathsParser
                 )
import           Oscoin.Crypto.Blockchain.Block (Beneficiary)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey as Crypto
import           Oscoin.P2P.Disco (discoOpts, discoParser)
import qualified Oscoin.P2P.Disco as P2P.Disco
import           Oscoin.Time (Duration, seconds)

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
    , optBlockTimeLower     :: Duration
    , optPaths              :: Paths
    , optEnvironment        :: Environment
    , optMetricsHost        :: Maybe HostName
    , optMetricsPort        :: Maybe PortNumber
    , optEkgHost            :: Maybe HostName
    , optEkgPort            :: Maybe PortNumber
    , optAllowEphemeralKeys :: Bool
    , optBeneficiary        :: Beneficiary crypto
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
       <> help "Beneficiary id for block rewards"
        )
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
        optBlockTimeLower
        optPaths
        optEnvironment
        optMetricsHost
        optMetricsPort
        optEkgHost
        optEkgPort
        optAllowEphemeralKeys
        optBeneficiary) = concat
    [ pure . Opt "host"        $ show optHost
    , pure . Opt "gossip-port" $ show optGossipPort
    , pure . Opt "api-port"    $ show optApiPort
    , discoOpts optDiscovery
    , pure . Opt "block-time-lower" . show . (`div` seconds) $ optBlockTimeLower
    , pathsOpts optPaths
    , environmentOpts optEnvironment
    , maybe [] (pure . Opt "metrics-host" . toS)  optMetricsHost
    , maybe [] (pure . Opt "metrics-port" . show) optMetricsPort
    , maybe [] (pure . Opt "ekg-host"     . toS)  optEkgHost
    , maybe [] (pure . Opt "ekg-port"     . show) optEkgPort
    , bool  [] [Flag "allow-ephemeral-keys"] optAllowEphemeralKeys
    , pure . Opt "beneficiary"  $ show optBeneficiary
    ]

renderNodeOptionsOpts
    :: Show (Crypto.ShortHash c)
    => P2P.Disco.CanRenderNetwork n
    => Options c n -> [Text]
renderNodeOptionsOpts = map (F.sformat F.build) . nodeOptionsOpts
