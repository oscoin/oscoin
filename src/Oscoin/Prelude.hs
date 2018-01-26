{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.Prelude
    ( module Foundation
    , module Control.Monad.Reader
    , module Data.Text
    , module Data.ByteString
    , module Data.Map
    , module Data.IORef
    , notImplemented
    , io
    , pass
    ) where

import Foundation
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.IORef (IORef)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT)

notImplemented :: a
notImplemented = error "Not implemented"

io :: MonadIO m => IO a -> m a
io = liftIO

pass :: Monad m => m ()
pass = pure ()
