module Oscoin.Account.Transaction
    ( Tx(..)
    , Receipt(..)
    , validateTransaction
    , applyTransaction
    , submitTransaction
    , verifySignature
    , setTx
    ) where

import           Oscoin.Prelude
import           Oscoin.Account
import           Oscoin.Address
import           Oscoin.Crypto.PubKey (PublicKey, Signed(..))
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Crypto.Hash (Hashed, Hashable, hash, toHex)
import qualified Oscoin.Node.State as State
import qualified Oscoin.Node.State.Mempool as Mempool

import qualified Data.ByteString.Base64.Extended as Base64
import           Data.Binary
import qualified Data.Text as T
import           Data.Aeson

type Coin = ()
type Patch = ()
type Permission = ()

type IssueId = ()
type AccountId = ()
type BranchId = ()
type RepoId = ()
type PatchId = ()

data Voice = Accept | Reject
    deriving (Show, Eq, Ord, Generic)

instance Binary Voice

-- | Transaction ID.
type instance Id Tx = Hashed Tx

-- | An account transaction.
data Tx =
      AccountTx     AccountId Address Coin [Permission]
    | PatchTx       RepoId BranchId PatchId [Patch]
    | VoiceIssueTx  IssueId Voice
    | AmmendIssueTx IssueId Text Text [PatchId]
    | SendTx        Address Address Coin
    | SetTx         AccId AccKey AccVal
    deriving (Show, Eq, Ord, Generic)

instance Binary Tx
instance Hashable Tx

instance ToJSON Tx where
    toJSON (SetTx acc key val) =
        object [ "object"  .= ("transaction" :: Text)
               , "type"    .= ("set" :: Text)
               , "account" .= acc
               , "key"     .= key
               , "value"   .= Base64.encodeLazy val
               ]
    toJSON _ =
        notImplemented

instance FromJSON Tx where
    parseJSON = withObject "Tx" $ \o -> do
        typ :: Text <- o .: "type"
        case typ of
            "set" -> do
                acc <- o .: "account"
                key <- o .: "key"
                val <- o .: "value"
                pure $ SetTx acc key (Base64.decodeLazy val)
            _ ->
                notImplemented

validateTransaction :: Signed Tx -> Either Error (Signed Tx)
validateTransaction stx@(Signed tx _) = do
    validateTransaction' tx
    pure stx

validateTransaction' :: Tx -> Either Error Tx
validateTransaction' tx@(SetTx accId accKey _)
  | not (T.null accId)
  , not (T.null accKey) = Right tx
  | otherwise = Left "Invalid account id or key"
validateTransaction' _ =
    notImplemented

setTx :: AccId -> AccKey -> AccVal -> Tx
setTx = SetTx

-------------------------------------------------------------------------------

-- | A transaction receipt. Contains the hashed transaction.
data Receipt tx = Receipt { fromReceipt :: Id tx }

instance Id tx ~ Hashed tx => ToJSON (Receipt tx) where
    toJSON (Receipt tx) =
        object [ "tx" .= decodeUtf8 (toHex tx) ]

-------------------------------------------------------------------------------

verifySignature :: PublicKey -> Signed Tx -> Either Error Tx
verifySignature pubKey stx =
    if Crypto.verify pubKey stx
       then Right (sigMessage stx)
       else Left  "Invalid signature"

-- | Apply a transaction to the account state-tree.
applyTransaction :: Tx -> AccTree -> AccTree
applyTransaction (SetTx acc key val) tree =
    setPath acc ["data", key] val tree
applyTransaction _ _ =
    notImplemented

-- | Submit a transaction to the mempool.
submitTransaction
    :: (Monad m, MonadSTM m, Hashable tx, Id tx ~ Hashed tx)
    => tx
    -> State.StorageT tx m (Receipt tx)
submitTransaction tx = do
    Mempool.addTxs [tx]
    pure $ Receipt (hash tx)
