{-# LANGUAGE OverloadedStrings #-}

module Oscoin.Git.Remote.GitArgs
    ( Args
    , parseArgs
    , gitArgGitDir
    , gitArgRemoteName
    , gitArgUrl
    )
where

import           Oscoin.Prelude

import           Data.Maybe
import qualified Data.Text as T
import           Oscoin.Git.Remote.Constants
import           System.Environment

data Args = Args { gitArgGitDir     :: FilePath
                 , gitArgRemoteName :: Text
                 , gitArgUrl        :: Text
                 } deriving (Show)

lookupGitDir :: IO FilePath
lookupGitDir = fromJust <$> lookupEnv "GIT_DIR"

parseArgs :: IO Args
parseArgs = do
    [remoteName, url] <- map T.pack <$> getArgs
    gitDir <- lookupGitDir
    pure $ Args
        { gitArgGitDir       = gitDir
        , gitArgRemoteName   = remoteName
        , gitArgUrl          = T.replace protocolPrefix "" url
        }
