{-# LANGUAGE OverloadedStrings #-}

module Oscoin.Git.Remote.Constants
    ( protocolName
    , protocolPrefix
    )
where

import           Data.Monoid
import           Data.Text

protocolName :: Text
protocolName = "oss"

protocolPrefix :: Text
protocolPrefix = protocolName <> "://"
