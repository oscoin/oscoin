module Oscoin.CLI.Command
    ( Command(..)
    , Result(..)
    , MonadCLI(..)
    , dispatchCommand
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Client as API
import           Oscoin.CLI.KeyStore
import           Oscoin.Consensus.Mining (mineGenesis)
import           Oscoin.Consensus.Nakamoto (PoW, mineNakamoto)
import           Oscoin.Crypto.Blockchain.Block
                 ( Beneficiary
                 , Block
                 , BlockHash
                 , Difficulty
                 , SealedBlock
                 , Unsealed
                 , emptyGenesisFromState
                 )
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Time (Timestamp)

import           Codec.Serialise (Serialise)
import           Crypto.Random.Types (MonadRandom)
import           Data.ByteArray (ByteArrayAccess)
import           Data.ByteArray.Orphans ()
import qualified Data.Yaml as Yaml


class (MonadRandom m, MonadKeyStore c m) => MonadCLI c m where
    -- | Sleep for given number of milliseconds
    sleep :: Int -> m ()
    -- | Print text to stdout
    putString :: Text -> m ()
    -- | Print text to stdout and add a newline
    putLine :: Text -> m ()
    -- | Get the current time
    getTime :: m Timestamp

    getClient :: m (API.Client c m)

data Result
    = ResultOk
    | ResultError Text

data Command c =
      GenerateKeyPair
    | GenesisCreate Difficulty (Beneficiary c)

dispatchCommand
    :: ( MonadCLI c m
       , Serialise (BlockHash c)
       , Crypto.HasDigitalSignature c
       , ByteArrayAccess (BlockHash c)
       , Yaml.ToJSON (SealedBlock c (Tx c) PoW)
       )
    => Command c
    -> m Result
dispatchCommand GenerateKeyPair = do
    kp <- Crypto.generateKeyPair
    writeKeyPair kp
    pure $ ResultOk

dispatchCommand (GenesisCreate d benef) = genesisCreate benef d

-- | Mine the genesis block print it as YAML to the console. Takes the
-- target difficulty and the beneficiary of the genesis block.
genesisCreate
    :: forall c m.
       ( MonadCLI c m
       , Serialise (BlockHash c)
       , ByteArrayAccess (BlockHash c)
       , Yaml.ToJSON (SealedBlock c (Tx c) PoW)
       )
    => Beneficiary c
    -> Difficulty
    -> m Result
genesisCreate benef diffi = do
    time <- getTime
    let unsealedGen :: Block c (Tx c) Unsealed = emptyGenesisFromState time benef (mempty :: LegacyTxState)
    result <- mineGenesis
        (mineNakamoto (Telemetry.probed Telemetry.noProbe) (const diffi)) unsealedGen
    case result of
        Left err  ->
            pure $ ResultError err
        Right gen -> do
            putLine . decodeUtf8 . Yaml.encode $ gen
            pure ResultOk
