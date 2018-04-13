{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.Prelude
    ( module Prelude
    , module Control.Applicative
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Trans.Class
    , module Control.Monad.STM.Class
    , module Control.Concurrent.STM
    , module Control.Monad.Fail
    , module Data.Semigroup
    , module Data.Traversable
    , module Data.Foldable
    , module Data.Text
    , module Data.ByteString
    , module Data.Map
    , module Data.IORef
    , module Data.Sequence
    , module Data.List.NonEmpty
    , module Data.Has
    , module Data.Either
    , module Data.Default
    , module Data.ByteArray
    , module Data.Function
    , module Data.Maybe
    , module Data.Functor
    , module Data.String
    , module Data.Word
    , module GHC.Stack
    , module GHC.Generics
    , module Debug.Trace
    , MonadIO
    , LByteString
    , Error(..)
    , Timestamp
    , Id
    , notImplemented
    , io
    , pass
    , encodeUtf8
    , decodeUtf8
    , tshow
    ) where

import Prelude hiding (fail)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Semigroup (Semigroup, (<>))
import Data.IORef (IORef)
import Control.Applicative (liftA2)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (Reader, MonadReader, ReaderT(..), runReaderT, ask, asks, local)
import Control.Monad.State (MonadState)
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Fail (MonadFail, fail)
import Data.Traversable (Traversable(..), sequence, traverse)
import Data.Foldable (for_, traverse_, Foldable, toList, null, foldr)
import Data.Sequence (Seq)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Has
import Data.Either (isRight, isLeft)
import Data.Default (def, Default)
import Data.ByteArray (ByteArrayAccess)
import Data.Function ((&))
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Functor (void)
import Data.String (IsString)
import Data.Word
import GHC.Stack (HasCallStack)
import GHC.Generics (Generic)
import Debug.Trace (trace, traceShow, traceM, traceShowM, traceShowId)

type LByteString = LBS.ByteString

newtype Error = Error { fromError :: Text }
    deriving (Show, Eq, IsString)

-- | Unix timestamp.
type Timestamp = Word32

-- | 'Id' maps a type to its identifier type. Example: 'Hashed' @tx@ for @tx@.
type family Id a :: *

notImplemented :: HasCallStack => a
notImplemented = error "Not implemented"

io :: MonadIO m => IO a -> m a
io = liftIO

pass :: Monad m => m ()
pass = pure ()

-- | Note that this is /not/ the standard @Data.Text.Encoding.decodeUtf8@. That
-- function will throw impure exceptions on any decoding errors. This function
-- instead uses @decodeLenient@.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

-- | Convert a value to readable Text.
tshow :: Show a => a -> Text
tshow = T.pack . Prelude.show
