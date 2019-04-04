module Oscoin.CLI.Parser
    ( CLI(..)
    , execParser
    , execParserPure
    ) where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Crypto.Blockchain.Block (minDifficulty, parseDifficulty)

import           Oscoin.CLI.Command
import           Oscoin.Configuration
                 ( ConfigPaths
                 , Environment
                 , Paths
                 , environmentParser
                 , pathsParser
                 )

import qualified Data.Text as T
import           Options.Applicative hiding (execParser, execParserPure)
import qualified Options.Applicative as Options

data CLI = CLI
    { cliPaths       :: Paths
    , cliEnvironment :: Environment
    , cliCommand     :: Command
    }

execParser :: ConfigPaths -> IO CLI
execParser cps = Options.execParser (mainParserInfo cps)

execParserPure :: ConfigPaths -> [String] -> ParserResult CLI
execParserPure cps = Options.execParserPure defaultPrefs (mainParserInfo cps)

mainParserInfo :: ConfigPaths -> ParserInfo CLI
mainParserInfo cps =
    info (helper <*> mainParser cps)
    $ progDesc "Oscoin CLI"


mainParser :: ConfigPaths -> Parser CLI
mainParser cps = CLI
    <$> pathsParser cps
    <*> environmentParser
    <*> subparser
        ( command "keypair"  (keyPairParser  `withInfo` "Key pair commands")
       <> command "genesis"  (genesisParser  `withInfo` "Genesis commands")
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
