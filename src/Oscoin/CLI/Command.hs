module Oscoin.CLI.Command
    ( Command(..)
    , dispatchCommand
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Client as API
import qualified Oscoin.API.Types as API
import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.KeyStore
import qualified Oscoin.CLI.Radicle as Rad
import           Oscoin.CLI.Revision
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (mkTx)

import           Crypto.Random.Types (MonadRandom)


data Command =
      RevisionCreate
    | RevisionList
    | RevisionMerge RevisionId
    | GenerateKeyPair
    deriving (Show)

type CommandContext m = (MonadRandom m , API.MonadClient m , MonadKeyStore m)

dispatchCommand :: CommandContext m => Command -> m (Result Text)
dispatchCommand RevisionCreate = submitTransaction createRevision
    where createRevision = Rad.fnApply "create-revision"
            [Rad.toRadicle emptyRevision]

dispatchCommand GenerateKeyPair = do
    kp <- Crypto.generateKeyPair
    writeKeyPair kp
    pure $ ResultOk

dispatchCommand _ = notImplemented

submitTransaction :: CommandContext m => Rad.Value -> m (Result Text)
submitTransaction rval = do
    tx <- signedTransaction rval
    result <- API.submitTransaction tx
    pure $ case result of
        API.Ok v    -> ResultValue (tshow v)
        API.Err err -> ResultError err

signedTransaction :: CommandContext m => Rad.Value -> m API.RadTx
signedTransaction v = do
    (pk, sk) <- readKeyPair
    msg <- Crypto.sign sk v
    pure $ mkTx msg pk
