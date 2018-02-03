{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.Prelude
    ( module Prelude
    , module Foundation
    , module Foundation.Collection
    , module Control.Monad.Reader
    , module Control.Monad.Trans.Class
    , module Data.Traversable
    , module Data.Foldable
    , module Data.Text
    , module Data.ByteString
    , module Data.Map
    , module Data.IORef
    , module Data.Sequence
    , module Data.List.NonEmpty
    , module Data.Has
    , module GHC.Stack
    , MonadIO
    , LByteString
    , Error(..)
    , notImplemented
    , io
    , pass
    ) where

import Prelude (read, map, zip)
-- TODO: We should be using these instead of the ones in "Foldable".
import Foundation hiding (NonEmpty, Foldable, toList, null)
import Foundation.Collection (getNonEmpty)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.IORef (IORef)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT, ask, asks, local)
import Data.Traversable (Traversable(..), sequence)
import Data.Foldable (for_, traverse_, Foldable, toList, null)
import Data.Sequence (Seq)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Has
import GHC.Stack (HasCallStack)

type LByteString = LBS.ByteString

newtype Error = Error Text
    deriving (Show, IsString)

notImplemented :: HasCallStack => a
notImplemented = error "Not implemented"

io :: MonadIO m => IO a -> m a
io = liftIO

pass :: Monad m => m ()
pass = pure ()
