{-# LANGUAGE OverloadedStrings #-}

module Oscoin.Git.Remote.IOUtil
    ( configureStdIO
    , getLines
    , createFile
    )
where

import           Oscoin.Prelude

import qualified Data.Text.IO as DTI
import           System.IO

configureStdIO :: IO ()
configureStdIO = do
  isTerminalDevice <- hIsTerminalDevice stdin
  if isTerminalDevice then pure ()
    else do
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering

getLines :: IO [Text]
getLines = do
    line <- DTI.getLine
    case line of
      "" -> pure []
      _  -> (line :) <$> getLines

createFile :: FilePath -> IO ()
createFile path = DTI.writeFile path ""
