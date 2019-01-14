module Oscoin.CLI.Parser
    ( execParser
    , execParserPure

    -- * Re-usable parsers
    , keyPathParser
    ) where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Crypto.Blockchain.Block (minDifficulty, parseDifficulty)

import           Oscoin.CLI.Command

import qualified Data.Text as T
import           Numeric.Natural
import           Options.Applicative hiding (execParser, execParserPure)
import qualified Options.Applicative as Options


execParser :: IO (Maybe FilePath, Command)
execParser = Options.execParser mainParserInfo

execParserPure :: [String] -> ParserResult (Maybe FilePath, Command)
execParserPure = Options.execParserPure defaultPrefs mainParserInfo

mainParserInfo :: ParserInfo (Maybe FilePath, Command)
mainParserInfo =
    info (helper <*> mainParser)
    $ progDesc "Oscoin CLI"


mainParser :: Parser (Maybe FilePath, Command)
mainParser =
    (,) <$> keyPathParser
        <*> subparser (
       command "revision" (revisionParser `withInfo` "Revision commands")
    <> command "keypair"  (keyPairParser  `withInfo` "Key pair commands")
    <> command "genesis"  (genesisParser  `withInfo` "Genesis commands")
    <> command "node"     (nodeParser     `withInfo` "Node commands")
    )

keyPathParser :: Parser (Maybe FilePath)
keyPathParser = optional (option str (
                             long "keys"
                          <> help ("The optional path to the folder containing the oscoin keys. " <>
                                   "If not specified, defaults to a path inside the Xdg directory.")
                          <> metavar "KEY-PATH (e.g. ~/.config/oscoin)"
                          ))


revisionParser :: Parser Command
revisionParser = subparser
    $  command "create" (revisionCreate `withInfo` "Create a revision")
    <> command "list"   (revisionList `withInfo` "List revisions")
    <> command "merge"  (revisionMerge `withInfo` "Merge a revision")
    where
        revisionCreate = RevisionCreate <$> confirmationsOption
        revisionList  = pure RevisionList
        revisionMerge = RevisionMerge <$> argument auto (metavar "REV-ID")

confirmationsOption :: Parser Natural
confirmationsOption = option auto
    (  long "confirmations"
    <> help "Number of block confirmations to wait for"
    <> value (3 :: Natural)
    <> showDefault
    <> metavar "N"
    )

nodeParser :: Parser Command
nodeParser = subparser $
    command "seed" (nodeSeed `withInfo` "Show the current node seed")
  where
    nodeSeed = NodeSeed <$> nodeSeedHost <*> nodeSeedPort
    nodeSeedHost = option str
        (  long "host"
        <> help "seed host"
        <> value "127.0.0.1"
        <> showDefault
        <> metavar "HOST"
        )
    nodeSeedPort = option auto
        (  long "port"
        <> help "seed port"
        <> value 6942
        <> showDefault
        <> metavar "PORT"
        )

keyPairParser :: Parser Command
keyPairParser = subparser
    $ command "generate" $ keyPairGenerate `withInfo`
        "Generate keypair to use with the other commands"
    where
        keyPairGenerate = pure GenerateKeyPair

genesisParser :: Parser Command
genesisParser =
    GenesisCreate <$> genesisFrom <*> genesisDifficulty
  where
    genesisFrom = many $ option str
        (  long "from"
        <> help ".rad input file"
        <> metavar "FILE"
        )
    genesisDifficulty = option (maybeReader (parseDifficulty . T.pack))
        (  long "difficulty"
        <> help "target difficulty"
        <> metavar "TARGET"
        <> value minDifficulty
        )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
