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
import           Oscoin.Consensus.Nakamoto (mineNakamoto)
import           Oscoin.Crypto.Blockchain.Block
                 (Beneficiary, BlockData, BlockHash, Difficulty)
import           Oscoin.Crypto.Blockchain.Eval (EvalError(..), buildGenesis)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Time (Timestamp)

import           Codec.Serialise (Serialise)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Crypto.Random.Types (MonadRandom)
import           Data.ByteArray (ByteArrayAccess)
import           Data.ByteArray.Orphans ()
import qualified Data.Yaml as Yaml


class (MonadRandom m, API.MonadClient c m, MonadKeyStore c m) => MonadCLI c m where
    -- | Sleep for given number of milliseconds
    sleep :: Int -> m ()
    -- | Print text to stdout
    putString :: Text -> m ()
    -- | Print text to stdout and add a newline
    putLine :: Text -> m ()
    -- | Read and parse a tx file.
    readTxFile :: FilePath -> m (Either Text (TxPayload c (Tx c)))
    -- | Get the current time
    getTime :: m Timestamp

data Result
    = ResultOk
    | ResultError Text

data Command c =
      GenerateKeyPair
    | GenesisCreate [FilePath] Difficulty (Beneficiary c)

dispatchCommand
    :: ( MonadCLI c m
       , Serialise (BlockHash c)
       , Serialise (Beneficiary c)
       , Serialise (Crypto.Signature c)
       , Serialise (Crypto.PublicKey c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , Crypto.HasDigitalSignature c
       , ByteArrayAccess (BlockHash c)
       , Yaml.ToJSON (Crypto.Hash c)
       , Yaml.ToJSON (Crypto.PublicKey c)
       , Yaml.ToJSON (Crypto.Signature c)
       , Yaml.ToJSON (BlockData c (Tx c))
       )
    => Command c
    -> m Result
dispatchCommand GenerateKeyPair = do
    kp <- Crypto.generateKeyPair
    writeKeyPair kp
    pure $ ResultOk

dispatchCommand (GenesisCreate [] d benef) =
    printGenesisYaml benef [] d
dispatchCommand (GenesisCreate files d benef) = do
    results <-
        for files $
            readTxFile >=> traverse signTransaction

    case partitionEithers results of
        (errs, signed)
            | null errs ->
                printGenesisYaml benef signed d
            | otherwise ->
                pure $ ResultError (mconcat errs)

-- | Mine a genesis block from a list of inputs and print it as YAML to
-- the console. Takes the target difficulty.
printGenesisYaml
    :: forall c m.
       ( MonadCLI c m
       , Serialise (BlockHash c)
       , Serialise (Beneficiary c)
       , Serialise (Crypto.Signature c)
       , Serialise (Crypto.PublicKey c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , ByteArrayAccess (BlockHash c)
       , Yaml.ToJSON (Crypto.Hash c)
       , Yaml.ToJSON (Crypto.Signature c)
       , Yaml.ToJSON (Crypto.PublicKey c)
       , Yaml.ToJSON (BlockData c (Tx c))
       )
    => Beneficiary c
    -> [Tx c]
    -> Difficulty
    -> m Result
printGenesisYaml benef txs diffi = do
    time <- getTime

    -- FIXME(adn) Pass a real evaluator and a real state.
    let dummyEval _ s = Right ([], s)
    case buildGenesis @c dummyEval time benef txs (mempty :: DummyEnv) of
        Left (_, err) ->
            pure $ ResultError (fromEvalError err)
        Right blk -> do
            result <- mineGenesis
                (mineNakamoto (Telemetry.probed Telemetry.noProbe) (const diffi)) blk

            case result of
                Left err  ->
                    pure $ ResultError err
                Right gen -> do
                    putLine . decodeUtf8 . Yaml.encode $ gen
                    pure ResultOk

signTransaction
    :: ( Crypto.HasDigitalSignature c
       , MonadCLI c m
       )
    => TxPayload c (Tx c)
    -> m (Tx c)
signTransaction payload = do
    (pk, sk) <- readKeyPair
    msg      <- Crypto.sign sk payload
    pure $ mkTx msg pk
