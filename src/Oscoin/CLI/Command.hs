module Oscoin.CLI.Command
    ( Command(..)
    , Result(..)
    , MonadCLI(..)
    , dispatchCommand
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Client as API
import qualified Oscoin.API.Types as API
import           Oscoin.CLI.KeyStore
import           Oscoin.Consensus.Mining (mineGenesis)
import           Oscoin.Consensus.Nakamoto (mineNakamoto)
import           Oscoin.Crypto.Blockchain.Block (BlockHash, Difficulty)
import           Oscoin.Crypto.Blockchain.Eval (EvalError(..), buildGenesis)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Data.RadicleTx as Rad
import           Oscoin.Data.Tx (mkTx)
import           Oscoin.Time (Timestamp)


import           Codec.Serialise (Serialise)
import qualified Crypto.Data.Auth.Tree.Internal as AuthTree
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
    -- | Read and parse a .rad file
    readRadFile :: FilePath -> m (Either Text Rad.Value)
    -- | Get the current time
    getTime :: m Timestamp

data Result
    = ResultOk
    | ResultError Text

data Command =
      GenerateKeyPair
    | GenesisCreate [FilePath] Difficulty
    deriving (Show)

dispatchCommand
    :: ( MonadCLI c m
       , Yaml.ToJSON (Crypto.PublicKey c)
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , Crypto.HasDigitalSignature c
       , Crypto.Hashable c (Crypto.PublicKey c)
       , ByteArrayAccess (BlockHash c)
       , Yaml.ToJSON (Crypto.Hash c)
       , Yaml.ToJSON (Crypto.Signature c)
       )
    => Command
    -> m Result
dispatchCommand GenerateKeyPair = do
    kp <- Crypto.generateKeyPair
    writeKeyPair kp
    pure $ ResultOk

dispatchCommand (GenesisCreate [] d) =
    printGenesisYaml [] d
dispatchCommand (GenesisCreate files d) = do
    results <-
        for files $
            readRadFile >=> traverse signTransaction

    case partitionEithers results of
        (errs, signed)
            | null errs ->
                printGenesisYaml signed d
            | otherwise ->
                pure $ ResultError (mconcat errs)

-- | Mine a genesis block from a list of inputs and print it as YAML to
-- the console. Takes the target difficulty.
printGenesisYaml
    :: forall c m.
       ( MonadCLI c m
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , Crypto.Hashable c (Crypto.PublicKey c)
       , ByteArrayAccess (BlockHash c)
       , Yaml.ToJSON (Crypto.Hash c)
       , Yaml.ToJSON (Crypto.PublicKey c)
       , Yaml.ToJSON (Crypto.Signature c)
       )
    => [API.RadTx c]
    -> Difficulty
    -> m Result
printGenesisYaml txs diffi = do
    time <- getTime

    case buildGenesis @c Rad.txEval time txs Rad.pureEnv of
        Left (_, err) ->
            pure $ ResultError (fromEvalError err)
        Right blk -> do
            result <- mineGenesis
                (mineNakamoto (const diffi)) blk

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
    => Rad.Value
    -> m (API.RadTx c)
signTransaction v = do
    (pk, sk) <- readKeyPair
    msg      <- Crypto.sign sk v
    pure $ mkTx msg pk
