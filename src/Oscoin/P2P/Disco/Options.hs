{-# LANGUAGE UndecidableInstances #-}

module Oscoin.P2P.Disco.Options
    ( Options(..)
    , discoParser
    , discoOpts
    , renderDiscoOpts

    , CanRenderNetwork
    , OptNetwork(..)
    , showOptNetwork

    , evalOptions
    , evalYesNo
    )
where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.Configuration as Global
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.P2P.Types
                 ( Network(..)
                 , SeedAddr
                 , pattern Somenet
                 , randomNetwork
                 , readNetwork
                 , readNodeAddr
                 , renderNetwork
                 , showNodeAddr
                 )

import qualified Data.Text as T
import qualified Formatting as F
import           Network.Socket (HostName, PortNumber)
import           Options.Applicative
import           Options.Applicative.Help
                 (paragraph, stringChunk, unChunk, vcatChunks, vsepChunks)
import           System.Console.Option (Opt(Flag, Opt))
import           System.IO (hFlush, stdout)
import           System.Random.SplitMix (newSMGen)
import           Text.Show (Show(..))

data Options crypto network = Options
    { optNetwork    :: network
    , optSeeds      :: [SeedAddr crypto]
    , optSDDomains  :: [HostName]
    , optEnableMDns :: Bool
    , optEnableGCE  :: Bool
    , optNameserver :: Maybe (HostName, PortNumber) -- only for testing currently
    } deriving (Generic)

deriving instance (Eq   (PublicKey c), Eq   n) => Eq   (Options c n)
deriving instance (Show (PublicKey c), Show n) => Show (Options c n)

data YesNo = Yes | No

data OptNetwork =
      Confirm   Text (YesNo -> Either String Network)
    | NoConfirm Network
    | Random

-- nb. Eq + Show for testing

instance Eq OptNetwork where
    (Confirm   a _) == (Confirm   b _) = a == b
    (NoConfirm a  ) == (NoConfirm b  ) = a == b
    Random          == Random          = True
    _               == _               = False

instance Show OptNetwork where
    show = showOptNetwork

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

evalYesNo :: a -> YesNo -> Either String a
evalYesNo x = \case
    Yes -> pure x
    No  -> Left "Cancelled"

evalOptions :: Options c OptNetwork -> IO (Either String (Options c Network))
evalOptions opt =
    map (\n -> opt { optNetwork = n })
        <$> evalOptNetwork (optNetwork opt)

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

discoParser :: Parser (Options c OptNetwork)
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

class CanRenderNetwork a where
    renderNetwork' :: a -> Maybe Text

instance CanRenderNetwork OptNetwork where
    renderNetwork' = \case
        Confirm n _ -> pure n
        NoConfirm n -> pure $ renderNetwork n
        _           -> Nothing

instance CanRenderNetwork Network where
    renderNetwork' = pure . renderNetwork

discoOpts :: CanRenderNetwork n => Options c n -> [Opt Text]
discoOpts
    (Options
        optNetwork
        optSeeds
        optSDDomains
        optEnableMDns
        optEnableGCE
        _optNameserver) = concat
    [ fromMaybe [] . map (pure . Opt "network") . renderNetwork' $ optNetwork
    , map (Opt "seed" . toS . showNodeAddr) optSeeds
    , map (Opt "sd-domain" . toS) optSDDomains
    , bool [] (pure (Flag "enable-mdns"))   optEnableMDns
    , bool [] (pure (Flag "enable-gce-sd")) optEnableGCE
    ]

renderDiscoOpts :: Options c OptNetwork -> [Text]
renderDiscoOpts = map (F.sformat F.build) . discoOpts
