{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Oscoin.Deployment.Internal.GCE
    ( MountPoint(fromMountPoint)
    , cloudConfig
    )
where

import           Oscoin.Prelude hiding ((<.>))

import           Oscoin.Deployment.Internal.CloudInit
import           Oscoin.Deployment.Internal.Docker
                 (ContainerConfig, RegistryLogin(..))
import           Oscoin.Deployment.Internal.Systemd
                 (dockerDaemonService, renderService)

import           Data.Generics.Product (the)
import           Data.List (nubBy)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Formatting as F
import           Lens.Micro (set, to, (.~))
import           Lens.Micro.Extras (view)
import           System.Console.Option
import           System.FilePath ((<.>), (</>))
import           System.Posix (groupReadMode, otherReadMode, ownerReadMode)

newtype MountPoint = MountPoint { fromMountPoint :: FilePath }
    deriving (Eq, Ord, Show, Read, IsString)

cloudConfig
    :: Word8
    -> Map Text (NonEmpty (User -> MountPoint) -> ContainerConfig)
    -> CloudConfig
cloudConfig (max 1 . min 8 -> numLocalDisks) containers =
    mkCloudConfig
        & the @"users"       .~ usrs
        & the @"write_files" .~ files
        & the @"bootcmd"     .~ bootcmds
        & the @"runcmd"      .~ cmds
  where
    localStorageMount = "/mnt/disks/ephemeral"

    containers' =
        let
            userDir = view (the @"name" . to toS)
            mnt usr = MountPoint . (</> userDir usr)
                    $ fromMountPoint localStorageMount
         in
            map ($ mnt :| []) containers

    usrs =
        Vector.fromList
            . nubBy (on (==) (view (the @"name")))
            . map (view (the @"user"))
            $ Map.elems containers'

    bootcmds =
        let
            mnt  = toS $ fromMountPoint localStorageMount
            fmt  = F.sformat F.build
         in
            Vector.fromList $ map fmt $
                [ Cmd @Text "mdadm"
                    ( Flag "create"
                    : Arg "/dev/md0"
                    : Opt "level" "0"
                    : Opt "raid-devices" (show numLocalDisks)
                    : Flag "force"
                    : map (\i -> Arg $ "/dev/nvme0n" <> show i)
                          [1 .. numLocalDisks]
                    )
                , Cmd "mkfs.ext4"
                    [ Arg "-F"
                    , Arg "/dev/md0"
                    ]
                , Cmd "mkdir"
                    [ Flag "parents"
                    , Arg mnt
                    ]
                , Cmd "mount"
                    [ Arg "/dev/md0"
                    , Arg mnt
                    ]
                , Cmd "chmod"
                    [ Arg "g+w"
                    , Arg mnt
                    ]
                ]

    -- FIXME: this should be a proper executable, a la coreos-metadata
    metadataHack =
        "echo GCE_LOCAL_IPV4=\
        \$(curl -Ss -H\"Metadata-Flavor: Google\" http://metadata.google.internal/computeMetadata/v1/instance/network-interfaces/0/ip) > \
        \/var/run/metadata"

    -- FIXME: support non-default topologies
    iptablesAllowVPCInternal =
        "iptables -w -A INPUT -p tcp -s 10.128.0.0/9 -j ACCEPT"

    cmds =
        let
            ephemeralUserDirs = flip concatMap usrs $ \usr ->
                let
                    mnt  = fromMountPoint localStorageMount
                    name = view (the @"name") usr
                    dir  = toS $ mnt </> toS name
                 in
                    [ Cmd @Text "mkdir" [Arg dir ]
                    , Cmd "chown"
                        [ Arg $ name <> ":" <> name
                        , Arg dir
                        ]
                    ]
            systemdReload = Cmd @Text "systemctl" [Arg "daemon-reload"]
            servicesStart = flip concatMap (Map.keys containers') $ \s ->
                let
                    cmd x =
                        Cmd @Text "systemctl"
                            [Arg x, Arg (s <> ".service")]
                 in
                    [cmd "enable", cmd "start"]

            fmt = F.sformat F.build
         in
            Vector.fromList
                $ metadataHack
                : iptablesAllowVPCInternal
                : map fmt ephemeralUserDirs
               <> map fmt (systemdReload : servicesStart)

    srvs =
        map ( set (the @"service" . the @"environmentFile")
                  (Just "/var/run/metadata")
            . uncurry (dockerDaemonService (pure GcrLogin))
            )
            (Map.toList containers')

    srvUnitFile srv =
        mkWriteFiles
            ("/etc/systemd/system/" </> toS (view (the @"name") srv) <.> "service")
            (fromPosixFileMode (ownerReadMode .|. groupReadMode .|. otherReadMode))
            rootOwner
            (plainTextFileContents $ renderService srv)

    fluentdSource container =
        mkWriteFiles
            ("/etc/stackdriver/logging.config.d/" </> toS container <.> "conf")
            (fromPosixFileMode (ownerReadMode .|. groupReadMode .|. otherReadMode))
            rootOwner
            . plainTextFileContents $ Text.unlines
                [ "<source>"
                , "  @type systemd"
                , "  filters [{\"CONTAINER_NAME\": \"" <> container <> "\"}]"
                , "  <storage>"
                , "    @type local"
                , "    persistent true"
                , "    path /var/log/google-fluentd/" <> container <> ".log.pos"
                , "  </storage>"
                , "  read_from_head true"
                , "  tag containers"
                , "</source>"
                ]

    files =
        Vector.fromList
            $ map fluentdSource (Map.keys containers)
           <> map srvUnitFile srvs
