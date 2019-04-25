module Oscoin.Deployment where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Deployment.Node as Node
import           Oscoin.Deployment.Options
import qualified Oscoin.P2P.Disco.Options as Disco

run :: Show (Crypto.ShortHash c) => Command c Disco.OptNetwork -> IO ()
run = \case
    DeployNode cmd -> Node.run cmd
