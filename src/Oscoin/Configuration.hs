-- | Umbrella module which gathers all the types which, when combined,
-- determines the particular configuration an Oscoin node is running with.
--
module Oscoin.Configuration
    ( ConfigPaths
    , getConfigPaths

    , Paths(..)
    , pathsParser
    , pathsOpts
    , renderPathsOpts

    , Environment(..)
    , allEnvironments
    , renderEnvironment
    , readEnvironment
    , readEnvironmentText
    , environmentParser
    , environmentOpts
    , renderEnvironmentOpts

    , Network(..)
    , allNetworks
    , renderNetwork
    , readNetworkText
    ) where

import           Oscoin.Prelude hiding (option, (<.>))

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise(..))
import           Control.Monad.Fail (fail)
import qualified Formatting as F
import           Options.Applicative
import           Paths_oscoin (getDataDir)
import           System.Console.Option (Opt(Opt))
import           System.Directory (XdgDirectory(..), getXdgDirectory)
import           System.FilePath ((<.>), (</>))

data ConfigPaths = ConfigPaths
    { xdgConfigHome :: FilePath
    , xdgConfigData :: FilePath
    , dataDir       :: FilePath
    } deriving Show

getConfigPaths :: IO ConfigPaths
getConfigPaths = ConfigPaths
    <$> getXdgDirectory XdgConfig "oscoin"
    <*> getXdgDirectory XdgData   "oscoin"
    <*> getDataDir

data Paths = Paths
    { keysDir               :: FilePath
    -- ^ Directory where the keypair is stored.
    --
    -- Default: $XDG_CONFIG_HOME/oscoin
    , blockstorePath        :: FilePath
    -- ^ Path to the block store SQLite .db file.
    --
    -- Default: $XDG_DATA_HOME/oscoin/blockstore.db
    , genesisParametersPath :: FilePath
    -- ^ Path to YAML encoded 'Oscoin.Crypto.Blockchain.Genesis.GenesisParameters'.
    --
    -- Default: $oscoin_datadir/data/genesis.yaml
    } deriving (Eq, Show, Generic)

pathsParser :: ConfigPaths -> Parser Paths
pathsParser ConfigPaths { xdgConfigHome, xdgConfigData, dataDir } = Paths
    <$> option str
        ( long "keys"
       <> help "Directory where the keypair is stored. \
               \Default: $XDG_CONFIG_HOME/oscoin"
       <> metavar "FILEPATH"
       <> value xdgConfigHome
       <> showDefault
        )
    <*> option str
        ( long "blockstore"
       <> help "Path to the block store SQLite .db file. \
               \Default: $XDG_DATA_HOME/oscoin/blockstore.db"
       <> metavar "FILEPATH"
       <> value (xdgConfigData </> "blockstore" <.> "db")
       <> showDefault
        )
    <*> option str
        ( long "genesis-parameters"
       <> help "Path to YAML encoded genesis parameters \
               \Default: $oscoin_datadir/data/genesis.yaml"
       <> metavar "FILEPATH"
       <> value (dataDir </> "data" </> "genesis" <.> "yaml")
       <> showDefault
        )

pathsOpts :: Paths -> [Opt Text]
pathsOpts (Paths keysDir blockstorePath genesisParametersPath) =
    [ Opt "keys"       . toS $ keysDir
    , Opt "blockstore" . toS $ blockstorePath
    , Opt "genesis-parameters" . toS $ genesisParametersPath
    ]

renderPathsOpts :: Paths -> [Text]
renderPathsOpts = map (F.sformat F.build) . pathsOpts

-- | Deployment environment
data Environment = Production | Development
    deriving (Show, Eq, Enum, Bounded)

-- | A list of all the possible environments.
allEnvironments :: [Environment]
allEnvironments = [minBound .. maxBound]

renderEnvironment :: Environment -> Text
renderEnvironment Production  = "production"
renderEnvironment Development = "development"

readEnvironment :: String -> Either String Environment
readEnvironment = readEnvironmentText . toS

readEnvironmentText :: Text -> Either String Environment
readEnvironmentText "production"  = pure Production
readEnvironmentText "development" = pure Development
readEnvironmentText x             = Left . toS $ "Unknown environment: " <> x

environmentParser :: Parser Environment
environmentParser = option (eitherReader readEnvironment)
    ( long "environment"
   <> help "The deployment environment"
   <> metavar (intercalate "|" (map (toS . renderEnvironment) allEnvironments))
   <> value Development
   <> showDefaultWith (toS . renderEnvironment)
    )

environmentOpts :: Environment -> [Opt Text]
environmentOpts = pure . Opt "environment" . renderEnvironment

renderEnvironmentOpts :: Environment -> [Text]
renderEnvironmentOpts = map (F.sformat F.build) . environmentOpts

-- | A logical oscoin network.
data Network =
      Mainnet
    | Testnet
    | Devnet
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

allNetworks :: [Network]
allNetworks = [minBound .. maxBound]

renderNetwork :: Network -> Text
renderNetwork Mainnet = "mainnet"
renderNetwork Testnet = "testnet"
renderNetwork Devnet  = "devnet"

readNetworkText :: Text -> Either String Network
readNetworkText "mainnet" = pure Mainnet
readNetworkText "testnet" = pure Testnet
readNetworkText "devnet"  = pure Devnet
readNetworkText x         = Left . toS $ "Unkown network: " <> x

instance Serialise Network where
    encode Mainnet =
           CBOR.encodeListLen 1
        <> CBOR.encodeWord 0
    encode Testnet =
           CBOR.encodeListLen 1
        <> CBOR.encodeWord 1
    encode Devnet =
           CBOR.encodeListLen 1
        <> CBOR.encodeWord 2

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLenCanonical CBOR.decodeWord16Canonical
        case pre of
            (1, 0) -> pure Mainnet
            (1, 1) -> pure Testnet
            (1, 2) -> pure Devnet
            e      -> fail $ "Failed decoding Network from CBOR: " ++ show e
