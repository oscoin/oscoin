{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

module Oscoin.Deployment.Node.GCE
    ( GceNodeOptions
    , gceNodeOptions
    , gceContainer
    )
where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Deployment.Internal.CloudInit
import           Oscoin.Deployment.Internal.Docker
import qualified Oscoin.Deployment.Internal.GCE as GCE
import qualified Oscoin.Node.Options as Node
import           Oscoin.P2P.Types (Network)

import           Data.Coerce (coerce)
import           Data.Generics.Product (the)
import           Data.IP (IP)
import qualified Data.List as List
import           Lens.Micro (over, set)
import           Network.Socket (HostName)
import           System.Console.Option

newtype GceNodeOptions c = GceNodeOptions [SomeOpt]

data OptError
    = NoSuchOption Text
    deriving Show

gceNodeOptions
    :: Show (Crypto.ShortHash c)
    => Node.Options c Network
    -> Either OptError (GceNodeOptions c)
gceNodeOptions nopt =
    let
        adjustments =
              set  (the @"optHost")                            ("0.0.0.0" :: IP)
            . set  (the @"optMetricsHost")                     (Nothing @HostName)
            . set  (the @"optEkgHost")                         (Nothing @HostName)
            . over (the @"optMetricsPort")                     (Just . fromMaybe 8081)
            . set  (the @"optPaths" . the @"blockstorePath")   "/storage/blockstore.db"
            . set  (the @"optDiscovery" . the @"optEnableMDns") False

        removals = removeOpt "keys" >=> removeOpt "genesis-parameters"

        -- FIXME: we should track option names at the type level (using e.g.
        -- @vinyl@)
        additions xs =
              ShOpt (Opt "metrics-host" (ShVar "GCE_LOCAL_IPV4"))
            : ShOpt (Opt "ekg-host"     (ShVar "GCE_LOCAL_IPV4"))
            : xs
     in
        map (GceNodeOptions . additions)
            . removals
            . map LitOpt
            . Node.nodeOptionsOpts
            $ adjustments nopt
  where
    hasOpt :: Text -> Opt a -> Bool
    hasOpt name = \case
        Opt x _ -> x == name
        Flag  x -> x == name
        _       -> False

    hasOpt' :: Text -> SomeOpt -> Bool
    hasOpt' name = \case
        LitOpt x -> hasOpt name x
        ShOpt  x -> hasOpt name x

    -- error if opt doesn't exist
    removeOpt :: Text -> [SomeOpt] -> Either OptError [SomeOpt]
    removeOpt name opts' = do
        idx <- note (NoSuchOption name) $ List.findIndex (hasOpt' name) opts'
        let (xs, ys) = List.splitAt idx opts'
         in pure $ xs <> drop 1 ys

gceContainer
    :: GceNodeOptions c
    -> Text
    -> NonEmpty (User -> GCE.MountPoint)
    -> ContainerConfig
gceContainer opts version (storage :| _) = ContainerConfig
    { user  = usr
    , env   = [mkEnvVar "HOME" "/home/oscoin"]
    , cmd   = coerce opts
    , image = Image
        { registry    = Just "eu.gcr.io"
        , repository  = Just "opensourcecoin"
        , name        = "oscoin"
        , tagOrDigest = ImageTag version
        }
    , workingDir = Just "/home/oscoin"
    , entrypoint = Just "/bin/oscoin"
    , hostConfig = HostConfig
        { logDriver      = JsonFile
        , networkMode    = NetworkHost
        , mount          =
            [ Mount
                { mountType = Bind
                , target    = "/home/oscoin"
                , source    = "/home/oscoin"
                , readOnly  = False
                }
            , Mount
                { mountType = Bind
                , target    = "/storage"
                , source    = toS . GCE.fromMountPoint $ storage usr
                , readOnly  = False
                }
            ]
        , capAdd         = []
        , capDrop        = []
        , oomScoreAdj    = Nothing
        , readonlyRootfs = False
        , securityOpt    = []
        }
    }
  where
    usr = mkUser "oscoin" & set (the @"uid") (Just 9000)
