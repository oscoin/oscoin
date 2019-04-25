module Main (main) where

import           Oscoin.Prelude

import           Oscoin.Configuration (ConfigPaths, getConfigPaths)
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.PubKey.RealWorld ()
import qualified Oscoin.Deployment as Deployment
import           Oscoin.Deployment.Options (Command, deploymentCommandParser)
import           Oscoin.P2P.Disco.Options (OptNetwork)

import           Options.Applicative

main :: IO ()
main = getConfigPaths >>= execParser . pinfo >>= Deployment.run
  where
    pinfo :: ConfigPaths -> ParserInfo (Command Crypto OptNetwork)
    pinfo cps =
        info (helper <*> deploymentCommandParser cps) $
            progDesc "Oscoin Deployment Utility" <> fullDesc
