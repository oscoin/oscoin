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

    , Network (Mainnet, Testnet, Devnet)
    , renderNetwork
    , readNetwork
    ) where

import           Oscoin.Prelude hiding (option, (<.>))

import           Data.String (IsString(..))
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
data Environment = Production | Development | Testing
    deriving (Show, Eq, Enum, Bounded)

-- | A list of all the possible environments.
allEnvironments :: [Environment]
allEnvironments = [minBound .. maxBound]

renderEnvironment :: Environment -> Text
renderEnvironment Development = "development"
renderEnvironment Production  = "production"
renderEnvironment Testing     = "testing"

readEnvironment :: String -> Either String Environment
readEnvironment = readEnvironmentText . toS

readEnvironmentText :: Text -> Either String Environment
readEnvironmentText "development" = pure Development
readEnvironmentText "production"  = pure Production
readEnvironmentText "testing"     = pure Testing
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
    | Somenet Text
    deriving (Eq, Show)

instance IsString Network where
    fromString = either (panic . toS) identity . readNetwork

renderNetwork :: Network -> Text
renderNetwork Mainnet     = "mainnet"
renderNetwork Testnet     = "testnet"
renderNetwork Devnet      = "devnet"
renderNetwork (Somenet x) = x

readNetwork :: String -> Either String Network
readNetwork "mainnet" = pure Mainnet
readNetwork "testnet" = pure Testnet
readNetwork "devnet"  = pure Devnet
readNetwork xs
  | length xs > 63           = Left "Network name longer than 63 characters"
  | Just c <- invalidChar xs = Left $ "Invalid character in network name: " <> [c]
  | otherwise                = Right $ Somenet (toS xs)
  where
    invalidChar = find (`elem` ("./:" :: String))
