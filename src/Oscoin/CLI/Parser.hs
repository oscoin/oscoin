module Oscoin.CLI.Parser
    ( CLI(..)
    , execParser
    , execParserPure
    ) where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Crypto.Blockchain.Block (minDifficulty, parseDifficulty)
import           Oscoin.Crypto.Hash (HasHashing(parseShortHash))

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

data CLI c = CLI
    { cliPaths       :: Paths
    , cliEnvironment :: Environment
    , cliCommand     :: Command c
    }

execParser :: HasHashing c => ConfigPaths -> IO (CLI c)
execParser cps = Options.execParser (mainParserInfo cps)

execParserPure :: HasHashing c => ConfigPaths -> [String] -> ParserResult (CLI c)
execParserPure cps = Options.execParserPure defaultPrefs (mainParserInfo cps)

mainParserInfo :: HasHashing c => ConfigPaths -> ParserInfo (CLI c)
mainParserInfo cps =
    info (helper <*> mainParser cps)
    $ progDesc "Oscoin CLI"


mainParser :: HasHashing c => ConfigPaths -> Parser (CLI c)
mainParser cps = CLI
    <$> pathsParser cps
    <*> environmentParser
    <*> subparser
        ( command "keypair"  (keyPairParser  `withInfo` "Key pair commands")
       <> command "genesis"  (genesisParser  `withInfo` "Genesis commands")
        )

keyPairParser :: Parser (Command c)
keyPairParser = subparser
    $ command "generate" $ keyPairGenerate `withInfo`
        "Generate keypair to use with the other commands"
    where
        keyPairGenerate = pure GenerateKeyPair

genesisParser :: HasHashing c => Parser (Command c)
genesisParser =
    GenesisCreate <$> genesisDifficulty <*> genesisBeneficiary
  where
    genesisDifficulty = option (maybeReader (parseDifficulty . T.pack))
        (  long "difficulty"
        <> help "target difficulty"
        <> metavar "TARGET"
        <> value minDifficulty
        )
    genesisBeneficiary = option (maybeReader (parseShortHash . T.pack))
        (  long "beneficiary"
        <> help "block reward beneficiary"
        <> metavar "ACCOUNT"
        )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
