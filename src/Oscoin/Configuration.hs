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

    , ConsensusOptions(..)
    , NakamotoLenientOptions(..)
    , consensusParser
    , consensusOpts
    , renderConsensusOptionsOpts

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
import           System.Console.Option (Opt(Flag, Opt))
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

data ConsensusOptions =
     NakamotoStrict
   | NakamotoLenient NakamotoLenientOptions
   deriving (Eq, Show)

data NakamotoLenientOptions = NakamotoLenientOptions
    { nloBlockTimeLower :: Word8
    , nloNoMining       :: Bool
    } deriving (Eq, Show)

consensusOpts :: ConsensusOptions -> [Opt Text]
consensusOpts NakamotoStrict =
    bool [] [Flag "nakamoto-consensus-strict"] True
consensusOpts (NakamotoLenient opts) =
    bool [] [Flag "nakamoto-consensus-lenient"] True <>
  ( Opt "block-time-lower" (show (nloBlockTimeLower opts))
  : bool [] [Flag "no-mining"] (nloNoMining opts)
  )

renderConsensusOptionsOpts :: ConsensusOptions -> [Text]
renderConsensusOptionsOpts = map (F.sformat F.build) . consensusOpts

consensusParser :: Parser ConsensusOptions
consensusParser = fromMaybe NakamotoStrict <$>
    optional (nakamotoStrictParser <|> nakamotoLenientParser)

nakamotoStrictParser :: Parser ConsensusOptions
nakamotoStrictParser =
    flag' NakamotoStrict (
            long "nakamoto-consensus-strict"
         <> help "Use 'Nakamoto' as consensus. This is strict in the sense \
                 \that it will try to enforce the full validation rules, \
                 \including the ones on block's age."
          )

nakamotoLenientParser :: Parser ConsensusOptions
nakamotoLenientParser =
    flag' NakamotoLenient (
            long "nakamoto-consensus-lenient"
         <> help "Use 'Nakamoto' as consensus. This is lenient in the sense \
                 \that it will not try to enforce the full validation rules, \
                 \but only a subset of them."
          ) <*> parseOptions
  where
    parseOptions = NakamotoLenientOptions <$>
                   option auto
                     ( long "block-time-lower"
                    <> help "(Lenient consensus only). Lower bound on the \
                            \ block time. Applies only to empty blocks in \
                            \the development environment, and is useful to avoid busy looping \
                            \in an idle network."
                    <> metavar "SECONDS"
                    <> value 1
                    <> showDefault
                     )
                <*> switch
                    ( long "no-mining"
                   <> help "(Lenient consensus only). Do not start the miner \
                           \ when the node is running."
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
