-- | Umbrella module which gathers all the types which, when combined,
-- determines the particular configuration an Oscoin node is running with.
--
module Oscoin.Configuration
    ( ConfigPaths
    , getConfigPaths

    , Paths(..)
    , pathsParser

    , Environment(..)
    , allEnvironments
    , renderEnvironment
    , readEnvironment
    , readEnvironmentText
    , environmentParser

    , Network(..)
    , allNetworks
    , renderNetwork
    , readNetworkText
    ) where

import           Oscoin.Prelude hiding (option, (<.>))

import           Options.Applicative
import           Paths_oscoin (getDataDir)
import           System.Directory (XdgDirectory(..), getXdgDirectory)
import           System.FilePath ((<.>), (</>))

data ConfigPaths = ConfigPaths
    { xdgConfigHome :: FilePath
    , xdgConfigData :: FilePath
    , dataDir       :: FilePath
    }

getConfigPaths :: IO ConfigPaths
getConfigPaths = ConfigPaths
    <$> getXdgDirectory XdgConfig "oscoin"
    <*> getXdgDirectory XdgData   "oscoin"
    <*> getDataDir

data Paths = Paths
    { keysDir        :: FilePath
    -- ^ Directory where the keypair is stored.
    --
    -- Default: $XDG_CONFIG_HOME/oscoin
    , blockstorePath :: FilePath
    -- ^ Path to the block store SQLite .db file.
    --
    -- Default: $XDG_DATA_HOME/oscoin/blockstore.db
    , genesisPath    :: FilePath
    -- ^ Path to the genesis YAML file.
    --
    -- Default: $oscoin_datadir/genesis.yaml
    }

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
        ( long "genesis"
       <> help "Path to the genesis.yaml file. \
               \Default: $oscoin_datadir/genesis.yaml"
       <> metavar "FILEPATH"
       <> value (dataDir </> "genesis" <.> "yaml")
       <> showDefault
        )

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
