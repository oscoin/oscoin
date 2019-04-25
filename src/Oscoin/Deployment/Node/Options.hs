{-# LANGUAGE DataKinds #-}

module Oscoin.Deployment.Node.Options where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Configuration (ConfigPaths)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Node.Options as Node
import qualified Oscoin.P2P.Disco.Options as Disco

import           Options.Applicative

data CloudProvider
    = GCE GceOptions

data GceOptions = GceOptions
    { gceNumSSDs :: Word8
    }

cloudProviderParser :: Parser CloudProvider
cloudProviderParser = hsubparser $
    command "gce"
            (info (GCE <$> gceOpts) (progDesc "Target Google Compute Engine"))
  where
    gceOpts = GceOptions
        <$> option (map (max 1 . min 8) auto)
            ( long    "num-ssds"
           <> help    "Number of local SSDs to provision (min 1, max 8)"
           <> metavar "INT"
           <> value   1
           <> showDefault
            )

data ConfigFormat
    = CloudInit
    deriving (Enum, Bounded)

allConfigFormats :: [ConfigFormat]
allConfigFormats = [minBound .. maxBound]

readConfigFormat :: String -> Either String ConfigFormat
readConfigFormat = \case
    "cloud-init" -> pure CloudInit
    x            -> Left $ "Unsupported configuration format: " <> x

showConfigFormat :: ConfigFormat -> String
showConfigFormat = \case
    CloudInit -> "cloud-init"

data Options crypto network = Options
    { optVersion     :: Text
    , optProvider    :: CloudProvider
    , optFormat      :: ConfigFormat
    , optOutput      :: Maybe FilePath
    , optNodeOptions :: Node.Options crypto network
    } deriving Generic

nodeDeployOptionsParser
    :: Crypto.HasHashing c
    => ConfigPaths
    -> Parser (Options c Disco.OptNetwork)
nodeDeployOptionsParser cps = Options
    <$> option str
        ( long    "version"
       <> help    "Version to deploy. Should be commit SHA1."
       <> metavar "STRING"
        )
    <*> cloudProviderParser
    <*> option (eitherReader readConfigFormat)
        ( long    "format"
       <> help    "Machine configuration format"
       <> metavar (intercalate "|" (map showConfigFormat allConfigFormats))
       <> value   CloudInit
       <> showDefaultWith showConfigFormat
        )
    <*> optional
        ( option str
          ( long    "output"
         <> help    "Store output artifacts here. Prints to stdout if not given."
         <> metavar "FILEPATH"
          )
        )
    <*> Node.nodeOptionsParser cps
