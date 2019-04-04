-- | Loads all the consensus-related config values from a static file.
module Oscoin.Consensus.Config
    ( Config(..)
    , configForEnvironment
    , defaultConfig
    ) where

import           Oscoin.Prelude

import           Oscoin.Configuration (Environment(..))
import           Oscoin.Crypto.Blockchain.Block (Depth)

-- | Consensus configuration parameters.
data Config = Config
    { maxBlockSize      :: Int
    -- ^ The maximum block size in bytes.
    , mutableChainDepth :: Depth
    -- ^ The depth, in blocks, of the \"mutable part\" of the chain,
    -- i.e. the part of the chain still subject to rollbacks.
    } deriving Show

defaultConfig :: Config
defaultConfig = Config
    { maxBlockSize = 1000000 -- 1 MB
    , mutableChainDepth = 2016 -- blocks
    }

configForEnvironment :: Environment -> Config
configForEnvironment = \case
    Production  -> defaultConfig
    Testing     -> defaultConfig
        { maxBlockSize      = 128000  -- 128kb
        , mutableChainDepth = 20      -- blocks
        }
    Development -> defaultConfig
        { maxBlockSize      = 512000  -- 512kb
        }
