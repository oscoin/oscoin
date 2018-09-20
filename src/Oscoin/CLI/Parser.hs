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
    $ progDesc "Oscoin CLI"

mainParser :: Parser Command
mainParser = subparser
    $  command "revision" (revisionParser `withInfo` "Revision commands")
    <> command "keypair"  (keyPairParser  `withInfo` "Key pair commands")

revisionParser :: Parser Command
revisionParser = subparser
    $  command "create" (revisionCreate `withInfo` "Create a revision")
    <> command "list"   (revisionList   `withInfo` "List revisions")
    <> command "merge"  (revisionMerge  `withInfo` "Merge a revision")
  where
    revisionCreate = pure RevisionCreate
    revisionList   = pure RevisionList
    revisionMerge  = RevisionMerge <$> argument auto (metavar "REV-ID")

keyPairParser :: Parser Command
keyPairParser = subparser $
    command "generate" $ keyPairGenerate `withInfo`
        "Generate keypair to use with the other commands"
  where
    keyPairGenerate = pure GenerateKeyPair

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
