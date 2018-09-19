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
    | RevisionStatus
    | GenerateKeyPair
    deriving (Show)

dispatchCommand
    :: ( MonadRandom m
       , API.MonadClient m
       , MonadKeyStore m
       )
    => Command -> m (Result Text)
dispatchCommand RevisionCreate = do
    tx <- createTransaction
    result <- API.submitTransaction tx
    pure $ case result of
        API.Ok v    -> ResultValue (tshow v)
        API.Err err -> ResultError err
  where
    createTransaction = do
        (pk, sk) <- readKeyPair
        let rev = emptyRevision
        let msgContent = Rad.fnApply "create-revision" [Rad.toRadicle rev]
        msg <- Crypto.sign sk msgContent
        pure $ mkTx msg pk

dispatchCommand GenerateKeyPair = do
    kp <- Crypto.generateKeyPair
    writeKeyPair kp
    pure $ ResultOk

dispatchCommand _ = notImplemented
