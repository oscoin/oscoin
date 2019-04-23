module Oscoin.P2P.Disco.Options
    ( Options(..)
    , discoParser

    , OptNetwork
    , showOptNetwork

    , evalOptions
    )
where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.Configuration as Global
import           Oscoin.Crypto (Crypto)
import           Oscoin.P2P.Types
                 ( Network(..)
                 , SeedAddr
                 , pattern Somenet
                 , randomNetwork
                 , readNetwork
                 , readNodeAddr
                 , renderNetwork
                 )

import qualified Data.Text as T
import           Network.Socket (HostName, PortNumber)
import           Options.Applicative
import           Options.Applicative.Help
                 (paragraph, stringChunk, unChunk, vcatChunks, vsepChunks)
import           System.IO (hFlush, stdout)
import           System.Random.SplitMix (newSMGen)

data Options network = Options
    { optNetwork    :: network
    , optSeeds      :: [SeedAddr Crypto]
    , optSDDomains  :: [HostName]
    , optEnableMDns :: Bool
    , optEnableGCE  :: Bool
    , optNameserver :: Maybe (HostName, PortNumber) -- only for testing currently
    }

data YesNo = Yes | No

data OptNetwork =
      Confirm   Text (YesNo -> Either String Network)
    | NoConfirm Network
    | Random

showOptNetwork :: OptNetwork -> String
showOptNetwork (Confirm s _) = toS $ "`" <> s <> "` (confirmation required)"
showOptNetwork (NoConfirm n) = toS $ renderNetwork n
showOptNetwork Random        = "randomly generated"

optNetworkReader :: ReadM OptNetwork
optNetworkReader = eitherReader $ map mkOpt . readNetwork
  where
    mkOpt = \case
        Mainnet      -> Confirm (renderNetwork Mainnet) $ evalYesNo Mainnet
        Testnet      -> Confirm (renderNetwork Testnet) $ evalYesNo Testnet
        Devnet       -> NoConfirm Devnet
        sn@Somenet{} -> NoConfirm sn

    evalYesNo x = \case
        Yes -> pure x
        No  -> Left "Cancelled"

evalOptions :: Options OptNetwork -> IO (Either String (Options Network))
evalOptions opt =
    map (\n -> opt { optNetwork = n }) <$> evalOptNetwork (optNetwork opt)

-- | Interactively obtain a 'YesNo' value.
promptYesNo :: Text -> IO YesNo
promptYesNo msg = prompt
  where
    prompt = do
        putStr msg *> hFlush stdout
        l <- T.toLower <$> getLine
        case T.uncons l of
            Just ('y',_) -> pure Yes
            Just ('n',_) -> pure No
            _            -> putStrLn (mempty @Text) *> hFlush stdout *> prompt

evalOptNetwork :: OptNetwork -> IO (Either String Network)
evalOptNetwork (NoConfirm n) = pure $ Right n
evalOptNetwork Random        = pure . randomNetwork <$> newSMGen
evalOptNetwork (Confirm s f) = f <$> promptYesNo msg
  where
    msg = "Are you sure you want to join network `" <> s <> "` [yes/no]:"

discoParser :: Parser (Options OptNetwork)
discoParser = Options
    <$> option optNetworkReader
        ( long "network"
       <> help "The name of the overlay network to join"
       <> metavar
            (intercalate
                "|" ("STRING" : map (toS . Global.renderNetwork) Global.allNetworks))
       <> value Random
       <> showDefaultWith showOptNetwork
        )
    <*> many
        ( option (eitherReader readNodeAddr)
            ( long "seed"
           <> helpDoc
               ( unChunk $ vsepChunks
               [ paragraph "Zero or more gossip seed nodes to connect to"
               , paragraph "If HOST is an IPv6 address, it must be enclosed in \
                           \square brackets to delimit it from the portnumber."
               , paragraph "If HOST is a domain name, all IPv4 and IPv6 \
                           \addresses bound to it will be considered as \
                           \distinct peer addresses."
               , paragraph "Examples:"
               , vcatChunks $ map stringChunk
                    [ "--seed=\"[2001:db8::01]:6942\""
                    , "--seed=\"127.0.0.1:6942\""
                    , "--seed=testnet.oscoin.io:6942\""
                    ]
               ]
               )
           <> metavar "HOST:PORT"
            )
        )
    <*> many
        ( option str
            ( long "sd-domain"
           <> helpDoc
                ( unChunk $ vsepChunks
                [ paragraph "Zero or more search domains to query for SRV records"
                , paragraph "Examples:"
                , vcatChunks $ map stringChunk
                    [ "--sd-domain=svc.cluster.local"
                    , "--sd-domain=oscoin.io"
                    , "--sd-domain=monadic.xyz"
                    ]
                ]
                )
           <> metavar "DOMAIN NAME"
            )
        )
    <*> switch
        ( long "enable-mdns"
       <> help "Enable mDNS discovery"
        )
    <*> switch
        ( long "enable-gce-sd"
       <> help "Enable instance discovery on Google Compute Engine"
        )
    <*> pure Nothing
