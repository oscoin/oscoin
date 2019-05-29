module Oscoin.CLI.Command
    ( Command(..)
    , Result(..)
    , MonadCLI(..)
    , dispatchCommand
    ) where

import           Oscoin.Prelude

import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import qualified Oscoin.API.Client as API
import           Oscoin.CLI.KeyStore
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Blockchain.Genesis (createGenesisParameters)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
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
       , Serialise (Beneficiary c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , Yaml.ToJSON (Crypto.ShortHash c)
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
       , Serialise (Beneficiary c)
       , ByteArrayAccess (BlockHash c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , Yaml.ToJSON (Crypto.ShortHash c)
       )
    => Beneficiary c
    -> Difficulty
    -> m Result
genesisCreate benef diffi = do
    time <- getTime
    let maybeGenesisParameters = createGenesisParameters benef time diffi
    case maybeGenesisParameters of
        Nothing ->
            pure $ ResultError "Failed to generate proof of work for genesis parameters"
        Just gen -> do
            putLine . decodeUtf8 . Yaml.encode $ gen
            pure ResultOk
