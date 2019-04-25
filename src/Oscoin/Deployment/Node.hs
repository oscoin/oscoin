{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

module Oscoin.Deployment.Node (run) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Deployment.Internal.CloudInit (renderCloudConfig)
import qualified Oscoin.Deployment.Internal.GCE as GCE (cloudConfig)
import           Oscoin.Deployment.Node.GCE
import           Oscoin.Deployment.Node.Options
import qualified Oscoin.P2P.Disco.Options as Disco

import           Data.Generics.Product (the)
import qualified Data.Text.Lazy.Builder as TBuild
import qualified Formatting as F
import           Lens.Micro (Lens, over)
import           Lens.Micro.Extras (view)

run :: Show (Crypto.ShortHash c) => Options c Disco.OptNetwork -> IO ()
run opts = do
    Options{..} <-
        -- Not great do this here instead of main, but I can not convince
        -- generic-lens that there is a type-changing lens focusing on the disco
        -- opts from the top-level deployment options.
        Disco.evalOptions (view discoOpts opts) >>=
            either (die . toS)
                   (\x -> pure $ over discoOpts (const x) opts)

    case optProvider of
        GCE GceOptions { gceNumSSDs } ->
            case optFormat of
                CloudInit ->
                    case gceNodeOptions optNodeOptions of
                        Left  e    -> die $ show e
                        Right nopt ->
                            printCloudConfig optOutput
                                . GCE.cloudConfig gceNumSSDs
                                $ [("oscoin", gceContainer nopt optVersion)]
  where
    discoOpts :: Lens (Options       c n) (Options       c m)
                      (Disco.Options c n) (Disco.Options c m)
    discoOpts = the @"optNodeOptions" . the @"optDiscovery"

    printCloudConfig out cfg =
        let rendered = TBuild.fromText $ renderCloudConfig cfg
         in case out of
                Nothing -> F.fprint F.builder rendered
                Just fp -> withFile fp WriteMode $ \hdl ->
                    F.hprint hdl F.builder rendered
