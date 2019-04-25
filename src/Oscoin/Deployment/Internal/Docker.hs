{-# LANGUAGE DataKinds #-}

module Oscoin.Deployment.Internal.Docker
    ( Image(..)
    , ImageTagOrDigest(..)
    , renderImage

    , RegistryLogin(..)
    , registryLoginCmd

    , EnvVar
    , mkEnvVar

    , ContainerConfig(..)
    , containerConfigOpts

    , HostConfig(..)
    , hostConfigOpts

    , LogDriver(..)
    , NetworkMode(..)
    , MountType(..)

    , Mount(..)
    , mountOpts
    )
where

import           Oscoin.Prelude

import           Oscoin.Deployment.Internal.CloudInit (User, renderUidOrName)

import           Crypto.Hash (Digest, SHA256)
import           Data.Generics.Product (the)
import qualified Data.Text as T
import           Lens.Micro.Extras (view)
import           Network.URI (URI, uriToString)
import           System.Console.Option
import           System.FilePath ((</>))

data ImageTagOrDigest
    = ImageTag    Text
    | ImageDigest (Digest SHA256) -- doesn't really make sense to use 'Digest' here

data Image = Image
    { registry    :: Maybe Text
    , repository  :: Maybe Text
    , name        :: Text
    , tagOrDigest :: ImageTagOrDigest
    } deriving Generic

renderImage :: Image -> Text
renderImage Image {..} =
    (<> renderTagOrDigest tagOrDigest)
    . mconcat . intersperse "/" . catMaybes
    $ [ registry
      , repository
      , Just name
      ]
  where
    renderTagOrDigest = \case
        ImageTag    t -> T.cons ':' t
        ImageDigest d -> "@sha256:" <> show d

data RegistryLogin
    = DockerLogin Text Var URI
    | GcrLogin

registryLoginCmd :: FilePath -> RegistryLogin -> SomeCmd
registryLoginCmd binPath = \case
    DockerLogin usr passv uri ->
        SomeCmd (binPath </> "docker")
                [ LitOpt $ Arg "login"
                , LitOpt $ Opt "username" usr
                , ShOpt  $ Opt "password" (ShVar passv)
                , LitOpt . Arg . toS $ uriToString identity uri ""
                ]

    GcrLogin ->
        SomeCmd (binPath </> "docker-credential-gcr")
                [LitOpt $ Arg "configure-docker"]

data EnvVar = EnvVar Text Text
    deriving Generic

mkEnvVar :: Text -> Text -> EnvVar
mkEnvVar = EnvVar

data ContainerConfig = ContainerConfig
    { user       :: User
    , env        :: [EnvVar]
    , cmd        :: [SomeOpt]
    , image      :: Image
    , workingDir :: Maybe FilePath
    , entrypoint :: Maybe FilePath
    , hostConfig :: HostConfig
    } deriving Generic

containerConfigOpts :: ContainerConfig -> [SomeOpt]
containerConfigOpts
    (ContainerConfig
        user
        env
        cmd
        image
        workingDir
        entrypoint
        hostConfig) =
    (<> cmd) . map LitOpt $
           Opt "user"
               ( renderUidOrName user
              -- Nb. assume default behaviour of useradd, which adds a group
              -- with gid=uid for every user. We need to specify it here to
              -- avoid the container user to have gid=0 (which is as good as
              -- running with uid=0).
              <> maybe mempty ((":" <>) . show) (uid user)
               )
{-
    FIXME: we need the numeric GID for this, but cloud-init won't give it to us

         : map (Opt "group-add" . renderGidOrName)
               (toList $ view (the @"groups") user)
-}
         : map (\(EnvVar k v) -> Opt "env" (k <> "=" <> v)) env
        <> maybeToList (Opt "workdir" . toS <$> workingDir)
        <> maybeToList (Opt "entrypoint" . toS <$> entrypoint)
        <> hostConfigOpts hostConfig
        <> [Arg (renderImage image)]
  where
    uid :: User -> Maybe Word16
    uid = view (the @"uid")

data HostConfig = HostConfig
    { logDriver      :: LogDriver
    , networkMode    :: NetworkMode
    , mount          :: [Mount]
    , capAdd         :: [Text]
    , capDrop        :: [Text]
    , oomScoreAdj    :: Maybe Int
    , readonlyRootfs :: Bool
    , securityOpt    :: [Text]
    -- we may want to tweak those at some point:
    -- , resources       :: ContainerResources
    } deriving Generic

hostConfigOpts :: HostConfig -> [Opt Text]
hostConfigOpts
    (HostConfig
        logDriver
        networkMode
        mount
        capAdd
        capDrop
        oomScoreAdj
        readonlyRootfs
        securityOpt) =

       [ Opt "log-driver" $ renderLogDriver   logDriver
       , Opt "network"    $ renderNetworkMode networkMode
       ]
    <> map (Opt "cap-add"     ) capAdd
    <> map (Opt "cap-drop"    ) capDrop
    <> map (Opt "security-opt") securityOpt
    <> concatMap mountOpts mount
    <> catMaybes
       [ bool Nothing (Just (Arg "readonly")) readonlyRootfs
       , Opt "oom-score-adj" . show <$> oomScoreAdj
       ]

data MountType
    = Bind
    | Volume
    | Tmpfs

renderMountType :: MountType -> Text
renderMountType = \case
    Bind   -> "bind"
    Volume -> "volume"
    Tmpfs  -> "tmpfs"

data Mount = Mount
    { mountType :: MountType
    , target    :: FilePath
    , source    :: FilePath
    , readOnly  :: Bool
    -- , consistency :: MountConsistency
    -- skipping options for now
    }

mountOpts :: Mount -> [Opt Text]
mountOpts (Mount mountType target source readOnly) =
    [ Opt "mount" . mconcat . intersperse "," $
        [ "type="     <> renderMountType mountType
        , "source="   <> toS source
        , "target="   <> toS target
        , "readonly=" <> bool "false" "true" readOnly
        ]
    ]

data LogDriver
    = JsonFile
    | Journald
    | LoggingDisabled

renderLogDriver :: LogDriver -> Text
renderLogDriver = \case
    JsonFile        -> "json-file"
    Journald        -> "journald"
    LoggingDisabled -> "none"

data NetworkMode
    = NetworkHost
    | NetworkDisabled

renderNetworkMode :: NetworkMode -> Text
renderNetworkMode = \case
    NetworkHost     -> "host"
    NetworkDisabled -> "none"

data HostPort = HostPort
    { hostIp   :: Text
    , hostPost :: Word16
    } deriving Generic
