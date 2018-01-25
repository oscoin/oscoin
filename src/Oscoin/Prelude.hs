{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.Prelude
    ( module Foundation
    , module Control.Monad.Reader
    , module Data.Text
    , module Data.ByteString
    , notImplemented
    , io
    ) where

import Foundation
import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT)

notImplemented :: a
notImplemented = error "Not implemented"

io :: MonadIO m => IO a -> m a
io = liftIO
