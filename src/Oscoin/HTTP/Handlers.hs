module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashable, Hashed, hash)
import           Oscoin.HTTP.Internal
import qualified Oscoin.Node as Node
import           Oscoin.Node.Mempool.Class (lookupTx, addTxs)
import qualified Oscoin.Crypto.PubKey as PubKey

import           Codec.Serialise hiding (encode)
import           Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import qualified Web.Spock as Spock
import qualified Data.ByteString.Lazy as LBS
import           Network.HTTP.Types.Status
import           Data.ByteString.Char8 (pack)

root :: ApiAction tx s i ()
root = respond ok200

getAllTransactions :: ToJSON tx => ApiAction tx s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respondJson ok200 mp

getTransaction :: (Hashable tx, ToJSON tx) => Hashed tx -> ApiAction tx s i ()
getTransaction txId = do
    mtx <- node (lookupTx txId)
    case mtx of
        Just tx -> respondJson ok200 tx
        Nothing -> respond notFound404

submitTransaction :: (Hashable tx, FromJSON tx, Serialise tx) => ApiAction tx s i ()
submitTransaction = do
    ct      <- getHeader "Content-Type"
    accept  <- getHeader "Accept"
    body    <- getRawBody
    submit (respond' accept) $ decode' ct body
  where
    submit _    (Left status) = respond status
    submit resp (Right tx) = do
      receipt <- node $ do
        addTxs [PubKey.unsign tx]
        pure $ Node.Receipt (hash tx)
      resp receipt

respond' :: (ToJSON a, Serialise a) => Maybe Text -> a -> ApiAction tx s i ()
respond' Nothing       _ = respond notAcceptable406
respond' (Just accept) a = do
  Spock.setHeader "Content-Type" accept
  case encode' accept a of
    Nothing -> respond notAcceptable406
    Just bs -> respondBytes accepted202 bs

decode' :: (Serialise tx, FromJSON tx) => Maybe Text -> LBS.ByteString -> Either Status tx
decode' ct
  | Just "application/cbor" <- ct = convert . deserialiseOrFail
  | Just "application/json" <- ct = convert . eitherDecode'
  | otherwise                     = const $ Left $ notAcceptable406
  where
    convert (Left err) = Left  $ mkStatus 400 $ pack $ show err
    convert (Right tx) = Right $ tx

encode' :: (Serialise a, ToJSON a) => Text -> a -> Maybe LBS.ByteString
encode' "application/json" = Just . encode
encode' "application/cbor" = Just . serialise
encode' _                  = const Nothing

-- | Runs a NodeT action in a MonadApi monad.
node :: MonadApi tx s i m => Node.NodeT tx s i IO a -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
