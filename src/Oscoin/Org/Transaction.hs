module Oscoin.Org.Transaction
    ( Tx
    , Receipt(..)
    , validateTransaction
    , applyTransaction
    , submitTransaction
    , verifySignature
    , setTx
    ) where

import           Oscoin.Prelude
import           Oscoin.Org
import           Oscoin.Address
import           Oscoin.Crypto.PubKey (PublicKey, Signed(..))
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Crypto.Hash (Hashed, Hashable, hash, toHex)
import qualified Oscoin.Node.State as State
import qualified Oscoin.Transaction.Mempool as Mempool

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

data Voice = Yea | Nay
    deriving (Show, Eq, Ord, Generic)

instance Binary Voice

-- | Transaction ID.
type instance Id Tx = Hashed Tx

-- | An org transaction.
data Tx =
      AccountTx     AccountId Address Coin [Permission]
    | PatchTx       RepoId BranchId PatchId [Patch]
    | VoiceIssueTx  IssueId Voice
    | AmmendIssueTx IssueId Text Text [PatchId]
    | SendTx        Address Address Coin
    | SetTx         OrgId OrgKey OrgVal
    deriving (Show, Eq, Ord, Generic)

instance Binary Tx
instance Hashable Tx

instance ToJSON Tx where
    toJSON (SetTx org key val) =
        object [ "object" .= ("transaction" :: Text)
               , "type"   .= ("set" :: Text)
               , "org"    .= org
               , "key"    .= key
               , "value"  .= Base64.encodeLazy val
               ]
    toJSON _ =
        notImplemented

instance FromJSON Tx where
    parseJSON = withObject "Tx" $ \o -> do
        typ :: Text <- o .: "type"
        case typ of
            "set" -> do
                org <- o .: "org"
                key <- o .: "key"
                val <- o .: "value"
                pure $ SetTx org key (Base64.decodeLazy val)
            _ ->
                notImplemented

validateTransaction :: Signed Tx -> Either Error (Signed Tx)
validateTransaction stx@(Signed tx _) = do
    validateTransaction' tx
    pure stx

validateTransaction' :: Tx -> Either Error Tx
validateTransaction' tx@(SetTx orgId orgKey _)
  | not (T.null orgId)
  , not (T.null orgKey) = Right tx
  | otherwise = Left "Invalid org id or key"
validateTransaction' _ =
    notImplemented

setTx :: OrgId -> OrgKey -> OrgVal -> Tx
setTx = SetTx

-------------------------------------------------------------------------------

-- | A transaction receipt. Contains the hashed transaction.
data Receipt tx = Receipt { fromReceipt :: Hashed tx }

instance ToJSON (Receipt tx) where
    toJSON (Receipt tx) =
        object [ "tx" .= decodeUtf8 (toHex tx) ]

-------------------------------------------------------------------------------

verifySignature :: PublicKey -> Signed Tx -> Either Error Tx
verifySignature pubKey stx =
    if Crypto.verify pubKey stx
       then Right (sigMessage stx)
       else Left  "Invalid signature"

-- | Apply a transaction to the org state-tree.
applyTransaction :: Tx -> OrgTree -> OrgTree
applyTransaction (SetTx org key val) tree =
    setPath org ["data", key] val tree
applyTransaction _ _ =
    notImplemented

-- | Submit a transaction to the mempool.
submitTransaction
    :: Hashable tx => tx -> State.StorageT tx IO (Receipt tx)
submitTransaction tx = do
    Mempool.updateMempool (Mempool.addTx (hash tx) tx)
    pure $ Receipt (hash tx)
