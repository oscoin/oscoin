module Oscoin.CLI.Parser
    ( execParser
    , execParserPure
    ) where

import           Oscoin.Prelude

import           Oscoin.CLI.Command

import           Options.Applicative hiding (execParser, execParserPure)
import qualified Options.Applicative as Options


execParser :: IO Command
execParser = Options.execParser mainParserInfo

execParserPure :: [String] -> ParserResult Command
execParserPure = Options.execParserPure defaultPrefs mainParserInfo

mainParserInfo :: ParserInfo Command
mainParserInfo =
    info (helper <*> mainParser)
    $ progDesc "Revision CLI"

mainParser :: Parser Command
mainParser = subparser $ createRevision


createRevision :: Mod CommandFields Command
createRevision =
    command "create"
    (info (helper <*> parser) (progDesc "Create a new revision"))
  where
    parser = pure RevisionCreate
