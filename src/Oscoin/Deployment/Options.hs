{-# LANGUAGE DataKinds #-}

module Oscoin.Deployment.Options where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Configuration (ConfigPaths)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Deployment.Node.Options as Node
import qualified Oscoin.P2P.Disco.Options as Disco

import           Options.Applicative

data Command crypto network
    = DeployNode (Node.Options crypto network)
    deriving Generic

deploymentCommandParser
    :: Crypto.HasHashing c
    => ConfigPaths
    -> Parser (Command c Disco.OptNetwork)
deploymentCommandParser cps = hsubparser $
    command "node" (info deployNode (progDesc "Deploy a Full Node"))
  where
    deployNode = DeployNode <$> Node.nodeDeployOptionsParser cps
