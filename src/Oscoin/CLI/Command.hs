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
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (mkTx)


import           Crypto.Random.Types (MonadRandom)

import           Radicle.Conversion
import qualified Radicle.Extended as Rad

class (MonadRandom m, API.MonadClient m, MonadKeyStore m) => MonadCLI m where
    -- | Sleep for given number of milliseconds
    sleep :: Int -> m ()
    -- | Print text to stdout and add a newline
    putLine :: Text -> m ()
    -- | Wraps an action with a spinner
    withSpinner :: Progress -> Int -> (Spinner -> m a) -> m a
    -- | Updates the progress of a spinner
    progress :: Spinner -> Progress -> m ()

data Result
    = ResultOk
    | ResultError Text

data Command =
      RevisionCreate Word64
    | RevisionList
    | RevisionMerge RevisionId
    | GenerateKeyPair
    deriving (Show)

dispatchCommand :: MonadCLI m => Command -> m Result
dispatchCommand (RevisionCreate confirmations) =
    submitTransaction createRevision confirmations
    where createRevision = Rad.fnApply "create-revision" [toRadicle emptyRevision]

dispatchCommand GenerateKeyPair = do
    kp <- Crypto.generateKeyPair
    writeKeyPair kp
    pure $ ResultOk

dispatchCommand cmd = pure $
    ResultError $ "Command `" <> show cmd <> "` not yet implemented"

-- | Waits until 'n' number of confirmations are reached for the given tx.
waitConfirmations
    :: MonadCLI m
    => Word64 -- ^ Number of confirmations to wait for.
    -> Int -- ^ Number of milliseconds to sleep between tx lookups.
    -> Hashed API.RadTx -- ^ Transaction hash to lookup.
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
submitTransaction :: MonadCLI m=> Rad.Value -> Word64 -> m Result
submitTransaction rval confirmations = do
    tx <- signTransaction rval
    API.submitTransaction tx >>= \case
        API.Err err -> pure $ ResultError err
        API.Ok (API.Receipt txHash) -> waitConfirmations confirmations 500 txHash

signTransaction :: MonadCLI m => Rad.Value -> m API.RadTx
signTransaction v = do
    (pk, sk) <- readKeyPair
    msg      <- Crypto.sign sk v
    pure $ mkTx msg pk
