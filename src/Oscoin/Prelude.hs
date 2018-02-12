{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.Prelude
    ( module Prelude
    , module Foundation
    , module Foundation.Collection
    , module Control.Monad.Reader
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
    , module GHC.Stack
    , module GHC.Generics
    , module Debug.Trace
    , MonadIO
    , LByteString
    , Error(..)
    , Id
    , notImplemented
    , io
    , pass
    , encodeUtf8
    , decodeUtf8
    , tshow
    ) where

import Prelude (read, map, zip, lookup)
import qualified Prelude as Prelude
-- TODO: We should be using these instead of the ones in "Foldable".
import Foundation hiding (Signed, NonEmpty, Foldable, toList, null, fail, (<>))
import Foundation.Collection (getNonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Semigroup (Semigroup, (<>))
import Data.IORef (IORef)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT, ask, asks, local)
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Fail (MonadFail, fail)
import Data.Traversable (Traversable(..), sequence)
import Data.Foldable (for_, traverse_, Foldable, toList, null)
import Data.Sequence (Seq)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Has
import Data.Either (isRight, isLeft)
import Data.Default (def, Default)
import Data.ByteArray (ByteArrayAccess)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Functor (void)
import GHC.Stack (HasCallStack)
import GHC.Generics (Generic)
import Debug.Trace (trace, traceShow, traceM, traceShowM)

type LByteString = LBS.ByteString

newtype Error = Error { fromError :: Text }
    deriving (Show, Eq, IsString)

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
