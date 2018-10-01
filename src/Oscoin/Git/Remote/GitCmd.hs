{-# LANGUAGE OverloadedStrings #-}

module Oscoin.Git.Remote.GitCmd
    ( GitCmd(..)
    , parseGitCmd
    )
where

import           Oscoin.Prelude

import qualified Data.Text as T

data GitCmd = GFetch
            | GPush
            | GOtherwise
            deriving (Read,  Show, Enum, Eq)

parseGitCmd :: Text -> GitCmd
parseGitCmd line
    | "fetch"   `T.isPrefixOf` line = GFetch
    | "push"    `T.isPrefixOf` line = GPush
    | otherwise                     = GOtherwise
