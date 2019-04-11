module Oscoin.API.HTTP.Internal
    ( ApiAction
    , Api
    , MonadApi
    , runApi
    , withHandle

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
import qualified Oscoin.Data.RadicleTx as RadicleTx
import qualified Oscoin.Node as Node

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List (init, isSuffixOf)
import qualified Data.Text as T
import           Network.HTTP.Types.Header (HeaderName)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
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
type ApiAction c s i =
    SpockAction (Node.Handle c (RadTx c) (RadicleTx.Env c) s i) () State

-- | The type of our api.
type Api c s i =
    SpockM (Node.Handle c (RadTx c) (RadicleTx.Env c) s i) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi c s i m =
    (HasSpock m, SpockConn m ~ Node.Handle c (RadTx c) (RadicleTx.Env c) s i)

-- | Create an empty state.
mkState :: State
mkState = State ()

getHeader :: HeaderName -> ApiAction c s i BS.ByteString
getHeader name = do
    header <- Spock.rawHeader name
    case header of
        Just v  -> pure v
        Nothing -> respondText HTTP.badRequest400 $ "Header " <> show name <> " is missing"

getRawBody :: ApiAction c s i LBS.ByteString
getRawBody = LBS.fromStrict <$> Spock.body

param' :: (FromHttpApiData p) => Text -> ApiAction c s i p
param' = Spock.param'

param :: (FromHttpApiData p) => Text -> ApiAction c s i (Maybe p)
param = Spock.param

listParam :: (FromHttpApiData p) => Text -> ApiAction c s i [p]
listParam key = do
    p <- param' key
    case decodeList p of
        Left err    -> respond HTTP.badRequest400 (errBody err)
        Right value -> pure value
  where
    decodeList = traverse parseQueryParam . split . inner
    inner = T.dropWhile (== '[') . T.dropWhileEnd (== ']')
    split = T.splitOn ","

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = Spock.runQuery

noBody :: Maybe ()
noBody = Nothing

errBody :: Text -> Result ()
errBody = Err

respond :: (Serialise a) => HTTP.Status -> a -> ApiAction c s i b
respond status value = do
    Spock.setStatus status
    Spock.setHeader "Content-Type" $ "application/cbor"
    Spock.lazyBytes $ Serialise.serialise value

respondText :: HTTP.Status -> Text -> ApiAction c s i b
respondText status bdy = do
    Spock.setStatus status
    Spock.setHeader "Content-Type" $ "text/plain"
    Spock.bytes $ encodeUtf8 bdy


getBody :: (Serialise a) => ApiAction c s i a
getBody = do
    rawCtHeader <- getHeader "Content-Type"
    when (rawCtHeader /= "application/cbor") $
        respondText HTTP.unsupportedMediaType415 "Unknown content type"
    body' <- getRawBody
    case Serialise.deserialiseOrFail body' of
        Left  _ -> respondText HTTP.badRequest400 "Failed to decode body"
        Right a -> pure a


runApi :: Api c s i ()
    -> Int
    -> Node.Handle c (RadTx c) (RadicleTx.Env c) s i
    -> IO ()
runApi app port hdl =
    runSpock port (mkMiddleware app hdl)

mkMiddleware
    :: Api c s i ()
    -> Node.Handle c (RadTx c) (RadicleTx.Env c) s i
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
