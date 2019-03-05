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
import           Oscoin.CLI.Revision
import           Oscoin.CLI.Spinner hiding (progress, withSpinner)
import           Oscoin.Consensus.Mining (mineGenesis)
import           Oscoin.Consensus.Nakamoto (mineNakamoto)
import           Oscoin.Crypto.Blockchain.Block (BlockHash, Difficulty)
import           Oscoin.Crypto.Blockchain.Eval (EvalError(..), buildGenesis)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Data.RadicleTx as Rad
import           Oscoin.Data.Tx (mkTx)
import           Oscoin.P2P (NodeAddr(..), mkNodeId)
import           Oscoin.Time (Timestamp)


import           Codec.Serialise (Serialise)
import qualified Crypto.Data.Auth.Tree.Internal as AuthTree
import           Crypto.Random.Types (MonadRandom)
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.Yaml as Yaml
import           Network.Socket (HostName, PortNumber)
import           Numeric.Natural

import           Radicle.Conversion
import qualified Radicle.Extended as Rad

class (MonadRandom m, API.MonadClient c m, MonadKeyStore c m) => MonadCLI c m where
    -- | Sleep for given number of milliseconds
    sleep :: Int -> m ()
    -- | Print text to stdout
    putString :: Text -> m ()
    -- | Print text to stdout and add a newline
    putLine :: Text -> m ()
    -- | Wraps an action with a spinner
    withSpinner :: Progress -> Int -> (Spinner -> m a) -> m a
    -- | Updates the progress of a spinner
    progress :: Spinner -> Progress -> m ()
    -- | Read and parse a .rad file
    readRadFile :: FilePath -> m (Either Text Rad.Value)
    -- | Get the current time
    getTime :: m Timestamp

data Result
    = ResultOk
    | ResultError Text

data Command =
      RevisionCreate Natural
    | RevisionList
    | RevisionMerge RevisionId
    | GenerateKeyPair
    | GenesisCreate [FilePath] Difficulty
    | NodeSeed HostName PortNumber
    deriving (Show)

dispatchCommand
    :: ( MonadCLI c m
       , Yaml.ToJSON (Crypto.PK c)
       , Serialise (BlockHash c)
       , Serialise (Crypto.PK c)
       , Serialise (Crypto.Signature c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , Crypto.HasDigitalSignature c
       , Crypto.Hashable c (Crypto.PK c)
       , ByteArrayAccess (BlockHash c)
       , Yaml.ToJSON (Crypto.Hash c)
       , Yaml.ToJSON (Crypto.Signature c)
       , Show (Crypto.PK c)
       , Show (Crypto.Signature c)
       , Show (Crypto.Hash c)
       )
    => Command
    -> m Result
dispatchCommand (RevisionCreate confirmations) =
    submitTransaction createRevision confirmations
    where createRevision = Rad.fnApply "create-revision" [toRad emptyRevision]

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

dispatchCommand (NodeSeed h p) = do
    (pk, _) <- readKeyPair
    putString . decodeUtf8 . Yaml.encode $
        NodeAddr { nodeId   = mkNodeId pk
                 , nodeHost = h
                 , nodePort = p
                 }
    pure ResultOk

dispatchCommand cmd = pure $
    ResultError $ "Command `" <> show cmd <> "` not yet implemented"

-- | Mine a genesis block from a list of inputs and print it as YAML to
-- the console. Takes the target difficulty.
printGenesisYaml
    :: forall c m.
       ( MonadCLI c m
       , Serialise (BlockHash c)
       , Serialise (Crypto.PK c)
       , Serialise (Crypto.Signature c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , Crypto.Hashable c (Crypto.PK c)
       , ByteArrayAccess (BlockHash c)
       , Yaml.ToJSON (Crypto.Hash c)
       , Yaml.ToJSON (Crypto.PK c)
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

-- | Waits until 'n' number of confirmations are reached for the given tx.
waitConfirmations
    :: ( Show (Crypto.PK c)
       , Show (Crypto.Signature c)
       , Show (Crypto.Hash c)
       , MonadCLI c m
       )
    => Natural                -- ^ Number of confirmations to wait for.
    -> Int                    -- ^ Number of milliseconds to sleep between tx lookups.
    -> Hashed c (API.RadTx c) -- ^ Transaction hash to lookup.
    -> m Result
waitConfirmations n delay txHash = withSpinner (msg 0) 100 go where
    msg n' = "Got " <> show n' <> " out of " <> show n <> " confirmations"
    go spinner = API.getTransaction txHash >>= \case
        API.Ok API.TxLookupResponse{txConfirmations} -> do
            progress spinner $ msg $ min txConfirmations $ min txConfirmations n
            if | txConfirmations < n -> sleep delay >> go spinner
               | otherwise -> pure ResultOk
        other -> pure $ ResultError $ "Unexpected response: " <> show other

-- | Submits a Radicle value to the API as signed transaction.
submitTransaction
    :: ( Show (Crypto.PK c)
       , Show (Crypto.Hash c)
       , Show (Crypto.Signature c)
       , Crypto.HasDigitalSignature c
       , MonadCLI c m
       )
    => Rad.Value
    -> Natural
    -> m Result
submitTransaction rval confirmations = do
    tx <- signTransaction rval
    API.submitTransaction tx >>= \case
        API.Err err -> pure $ ResultError err
        API.Ok (API.TxSubmitResponse txHash) -> waitConfirmations confirmations 500 txHash

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
