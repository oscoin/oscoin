module Oscoin.API.HTTP.Internal
    ( ApiAction
    , Api
    , ApiTx
    , runApi
    , runApi'
    , liftNode

    , trailingSlashPolicy
    , indexPolicy
    , mkMiddleware

    , errBody
    , respond
    , noBody

    , param
    , param'
    , listParam
    , getBody
    ) where

import           Oscoin.Prelude hiding (State, state)

import           Oscoin.API.Types
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.Query
import           Oscoin.Data.Tx.Abstract
import qualified Oscoin.Node as Node
import           Oscoin.Telemetry.Logging (Logger, debug, ftag, shown, (%))

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List (init, isSuffixOf)
import qualified Data.Text as T
import           Network.HTTP.Types.Header (HeaderName)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Static as Wai
import           Web.HttpApiData (FromHttpApiData, parseQueryParam)
import           Web.Spock
                 (HasSpock, SpockAction, SpockConn, SpockM, runSpock, spock)
import qualified Web.Spock as Spock
import           Web.Spock.Config
                 ( ConnBuilder(..)
                 , PoolCfg(..)
                 , PoolOrConn(..)
                 , defaultSpockCfg
                 )

-- | The global server state.
data State = State ()
    deriving (Show)

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction c tx s i =
    SpockAction (Node.Handle c tx s i) () State

-- | The type of our api.
type Api c tx s i =
    SpockM (Node.Handle c tx s i) () State

-- | Contraints on the transaction parameter of the API that need to be
-- satisfied to run the API.
type ApiTx c tx =
    ( Serialise tx
    , Crypto.Hashable c tx
    , Query (TxState c tx)
    , Serialise (TxOutput c tx)
    )

-- | Create an empty state.
mkState :: State
mkState = State ()

getHeader :: HeaderName -> ApiAction c tx s i BS.ByteString
getHeader name = do
    header <- Spock.rawHeader name
    case header of
        Just v  -> pure v
        Nothing -> respondText HTTP.badRequest400 $ "Header " <> show name <> " is missing"

getRawBody :: ApiAction c tx s i LBS.ByteString
getRawBody = LBS.fromStrict <$> Spock.body

param' :: (FromHttpApiData p) => Text -> ApiAction c tx s i p
param' = Spock.param'

param :: (FromHttpApiData p) => Text -> ApiAction c tx s i (Maybe p)
param = Spock.param

listParam :: (FromHttpApiData p) => Text -> ApiAction c tx s i [p]
listParam key = do
    p <- param' key
    case decodeList p of
        Left err    -> respond HTTP.badRequest400 (errBody err)
        Right value -> pure value
  where
    decodeList = traverse parseQueryParam . split . inner
    inner = T.dropWhile (== '[') . T.dropWhileEnd (== ']')
    split = T.splitOn ","

-- | Runs a NodeT action in a MonadApi monad.
liftNode
    :: (HasSpock m, SpockConn m ~ Node.Handle c tx s i)
    => Node.NodeT c tx s i IO a
    -> m a
liftNode s = Spock.runQuery $ \h ->
    Node.runNodeT h s

noBody :: Maybe ()
noBody = Nothing

errBody :: Text -> Result ()
errBody = Err

respond :: (Serialise a) => HTTP.Status -> a -> ApiAction c tx s i b
respond status value = do
    Spock.setStatus status
    Spock.setHeader "Content-Type" $ "application/cbor"
    Spock.lazyBytes $ Serialise.serialise value

respondText :: HTTP.Status -> Text -> ApiAction c tx s i b
respondText status bdy = do
    Spock.setStatus status
    Spock.setHeader "Content-Type" $ "text/plain"
    Spock.bytes $ encodeUtf8 bdy


getBody :: (Serialise a) => ApiAction c tx s i a
getBody = do
    rawCtHeader <- getHeader "Content-Type"
    when (rawCtHeader /= "application/cbor") $
        respondText HTTP.unsupportedMediaType415 "Unknown content type"
    body' <- getRawBody
    case Serialise.deserialiseOrFail body' of
        Left  _ -> respondText HTTP.badRequest400 "Failed to decode body"
        Right a -> pure a


runApi :: Api c tx s i ()
    -> Int
    -> Node.Handle c tx s i
    -> IO ()
runApi app port hdl =
    runSpock port (mkMiddleware app hdl)

-- | Generic function to run the 'Api'. It takes as input a 'Logger', a
-- callback to be used after the listening socket is ready and a 'Bool' to
-- whether or not display the banner at startup.
runApi'
    :: Logger
    -- ^ A 'Logger'.
    -> IO ()
    -- ^ An action to be called when the listening socket is ready.
    -> Api c tx s i ()
    -- ^ The 'Api'.
    -> Int
    -- ^ The listening port.
    -> Node.Handle c tx s i
    -- ^ The node 'Handle'.
    -> IO ()
runApi' logger onReady app port hdl = do
    debug logger "Spock is running" (ftag "port" % shown) port
    waiApp <- Spock.spockAsApp (mkMiddleware app hdl)
    let settings = Warp.setPort port Warp.defaultSettings
    Warp.runSettings (Warp.setBeforeMainLoop onReady settings) waiApp

mkMiddleware
    :: Api c tx s i ()
    -> Node.Handle c tx s i
    -> IO Wai.Middleware
mkMiddleware app hdl = do
    spockCfg <- defaultSpockCfg () (PCConn connBuilder) state
    spock spockCfg app
  where
    connBuilder = ConnBuilder (pure hdl) (const pass) (PoolCfg 1 1 30)
    state       = mkState

-- | Static Policy to serve index.html files from directories.
indexPolicy :: String -> Wai.Policy
indexPolicy path =
    Wai.policy $ pure . change path (path ++ "/index.html")
  where
    change from to = (== from) >>= bool identity (const to)

-- | Static Policy to remove trailing slashes.
trailingSlashPolicy :: Wai.Policy
trailingSlashPolicy =
    Wai.policy f
  where
    f s | "/" `isSuffixOf` s = Just (init s)
        | otherwise          = Just s
