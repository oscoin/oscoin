-- | Loads all the protocol-related config values from a static file.
{-# LANGUAGE TemplateHaskell #-}
module Oscoin.ProtocolConfig (
      ProtocolConfig(..)
    , getProtocolConfig
    ) where

import           Oscoin.Prelude

import           Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

import           Oscoin.Environment
import           Paths_oscoin

-- | The 'FilePath' which hosts the configuration file.
configPath :: FilePath
configPath = "data/config.yaml"


data ProtocolConfig = ProtocolConfig {
      maxBlockSize :: !Int
    -- ^ The maximum block size in MB.
    } deriving Show

deriveFromJSON defaultOptions ''ProtocolConfig

-- | Lookup the maximum size for a block (in MBs).
-- TODO(adn): Use a proper unit of measure.
getProtocolConfig :: Environment -> IO ProtocolConfig
getProtocolConfig env = do

    -- First, parse the yaml file as a 'Value' blob, the access the relevant
    -- environment subsection and only at that point try to reify the blob
    -- back into a 'ProtocolConfig' data structure.
    configFile <- getDataFileName configPath
    blob <- Yaml.decodeFileThrow configFile

    -- The lookup is done by using the environment as a lookup key.
    case HM.lookup (toLookupKey env) (blob :: Yaml.Object) of
         Nothing -> die $  T.pack $ show env
                        <> " not found. Possible values: "
                        <> intercalate "," (map show allEnvironments)
         Just configMap ->
             case Yaml.parseEither (const (Yaml.parseJSON configMap)) () of
                  Left err -> die (T.pack err)
                  Right cp -> pure cp

  where
    toLookupKey :: Environment -> Text
    toLookupKey Development = "development"
    toLookupKey Production  = "production"
    toLookupKey Testing     = "testing"



