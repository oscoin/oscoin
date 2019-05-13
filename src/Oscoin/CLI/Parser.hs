module Oscoin.CLI.Parser
    ( CLI(..)
    , execParser
    , execParserPure
    ) where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Crypto.Address (decodeAddress, toPublicKey)
import           Oscoin.Crypto.Blockchain.Block
                 (Beneficiary, minDifficulty, parseDifficulty)

import           Oscoin.CLI.Command
import           Oscoin.Configuration
                 ( ConfigPaths
                 , Environment
                 , Paths
                 , environmentParser
                 , pathsParser
                 )

import           Codec.Serialise (Serialise)
import           Data.Bifunctor (first)
import qualified Data.Text as T
import           Options.Applicative hiding (execParser, execParserPure)
import qualified Options.Applicative as Options

data CLI c = CLI
    { cliPaths       :: Paths
    , cliEnvironment :: Environment
    , cliCommand     :: Command c
    }

execParser :: Serialise (Beneficiary c) => ConfigPaths -> IO (CLI c)
execParser cps = Options.execParser (mainParserInfo cps)

execParserPure :: Serialise (Beneficiary c) => ConfigPaths -> [String] -> ParserResult (CLI c)
execParserPure cps = Options.execParserPure defaultPrefs (mainParserInfo cps)

mainParserInfo :: Serialise (Beneficiary c) => ConfigPaths -> ParserInfo (CLI c)
mainParserInfo cps =
    info (helper <*> mainParser cps)
    $ progDesc "Oscoin CLI"


mainParser :: Serialise (Beneficiary c) => ConfigPaths -> Parser (CLI c)
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

genesisParser :: Serialise (Beneficiary c) => Parser (Command c)
genesisParser =
    GenesisCreate <$> genesisFrom <*> genesisDifficulty <*> genesisBeneficiary
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
    genesisBeneficiary = option (eitherReader addrToPubKey)
        (  long "beneficiary"
        <> help "block reward beneficiary"
        <> metavar "ADDRESS"
        )
    addrToPubKey s =
        toPublicKey <$> first show ((decodeAddress . encodeUtf8 . T.pack) s)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
