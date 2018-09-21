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

data Result
    = ResultOk
    | ResultError Text

data Command =
      RevisionCreate
    | RevisionList
    | RevisionMerge RevisionId
    | GenerateKeyPair
    deriving (Show)

dispatchCommand :: MonadCLI m => Command -> m Result
dispatchCommand RevisionCreate = submitTransaction createRevision
    where
        createRevision =
            Rad.fnApply "create-revision" [toRadicle emptyRevision]

dispatchCommand GenerateKeyPair = do
    kp <- Crypto.generateKeyPair
    writeKeyPair kp
    pure $ ResultOk

dispatchCommand cmd = pure $
    ResultError $ "Command `" <> show cmd <> "` not yet implemented"

submitTransaction :: MonadCLI m => Rad.Value -> m Result
submitTransaction rval = do
    tx <- signTransaction rval
    result <- API.submitTransaction tx
    case result of
        API.Ok v    -> do putLine (show v)
                          pure $ ResultOk
        API.Err err -> pure $ ResultError err

signTransaction :: MonadCLI m => Rad.Value -> m API.RadTx
signTransaction v = do
    (pk, sk) <- readKeyPair
    msg <- Crypto.sign sk v
    pure $ mkTx msg pk
