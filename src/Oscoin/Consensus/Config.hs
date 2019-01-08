-- | Loads all the consensus-related config values from a static file.
module Oscoin.Consensus.Config (
      Config(..)
    , getConfig
    ) where

import           Oscoin.Prelude

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

import           Oscoin.Environment
import           Paths_oscoin

-- | The 'FilePath' which hosts the configuration file.
configPath :: FilePath
configPath = "data/config.yaml"


data Config = Config {
      maxBlockSize :: !Int
    -- ^ The maximum block size in MB.
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        maxBlockSize <- o .: "maxBlockSize"
        pure Config{..}

-- | Lookup the maximum size for a block (in MBs).
-- TODO(adn): Use a proper unit of measure.
getConfig :: Environment -> IO Config
getConfig env = do

    -- First, parse the yaml file as a 'Value' blob, the access the relevant
    -- environment subsection and only at that point try to reify the blob
    -- back into a 'Config' data structure.
    configFile <- getDataFileName configPath
    blob <- Yaml.decodeFileThrow configFile

    -- The lookup is done by using the environment as a lookup key.
    case HM.lookup (toText env) (blob :: Yaml.Object) of
         Nothing -> die $  T.pack $ show env
                        <> " not found. Possible values: "
                        <> intercalate "," (map show allEnvironments)
         Just configMap ->
             case Yaml.parseEither (const (Yaml.parseJSON configMap)) () of
                  Left err -> die (T.pack err)
                  Right cp -> pure cp



