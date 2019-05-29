{-# LANGUAGE DataKinds #-}

module Oscoin.Deployment.Internal.Systemd
    ( Unit
    , mkUnit
    , multiUserTarget
    , gcrOnlineTarget

    , UnitSection
    , ServiceSection
    , InstallSection

    , Service
    , renderService
    , dockerDaemonService
    )
where

import           Oscoin.Prelude

import qualified Oscoin.Deployment.Internal.Docker as Doge

import           Data.Generics.Product (the)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Formatting as F
import           Lens.Micro.Extras (view)
import           System.Console.Option

data Unit
    = KnownUnit Text
    | Unit      Text UnitSection

multiUserTarget :: Unit
multiUserTarget = KnownUnit "multi-user.target"

gcrOnlineTarget :: Unit
gcrOnlineTarget = KnownUnit "gcr-online.target"

mkUnit :: Text -> UnitSection -> Unit
mkUnit = Unit

data UnitSection = UnitSection
    { description :: Maybe Text
    , wants       :: [Unit]
    , before      :: [Unit]
    , after       :: [Unit]
    } deriving Generic

data ServiceSection = ServiceSection
    { environment     :: Map Text Text
    , environmentFile :: Maybe FilePath
    , execStartPre    :: Maybe Text
    , execStart       :: Text
    , execStop        :: Maybe Text
    , execStopPost    :: Maybe Text
    } deriving Generic

data InstallSection = InstallSection
    { wantedBy   :: [Unit]
    , requiredBy :: [Unit]
    , also       :: [Unit]
    } deriving Generic

data Service = Service
    { name    :: Text
    , unit    :: UnitSection
    , service :: ServiceSection
    , install :: Maybe InstallSection
    } deriving Generic

dockerDaemonService
    :: Maybe Doge.RegistryLogin
    -> Text
    -> Doge.ContainerConfig
    -> Service
dockerDaemonService rlogin name container =
    Service
        name
        UnitSection
            { description = Just name
            , wants       = depends
            , after       = depends
            , before      = mempty
            }
        ServiceSection
            { environment = Map.fromList
                [("HOME", "/home/" <> view (the @"user" . the @"name") container)]

            , environmentFile = Nothing

            , execStartPre = fmtTxt .
                Doge.registryLoginCmd "/usr/bin" <$> rlogin

            , execStart    = fmtTxt
                . SomeCmd "/usr/bin/docker"
                $ LitOpt (Arg "run")
                : LitOpt (Opt "name" name)
                : Doge.containerConfigOpts container

            , execStop     = Just . fmtTxt $
                Cmd @Text "/usr/bin/docker" [Arg "stop", Arg name]

            , execStopPost = Just . fmtTxt $
                Cmd @Text "/usr/bin/docker" [Arg "rm", Arg name]
            }
        $ pure InstallSection
            { wantedBy   = depends
            , requiredBy = mempty
            , also       = mempty
            }
  where
    fmtTxt :: F.Buildable a => a -> Text
    fmtTxt = F.sformat F.build

    depends = maybeToList $ rlogin <&> \case
        Doge.GcrLogin -> gcrOnlineTarget
        _             -> multiUserTarget

renderService :: Service -> Text
renderService Service {..} = T.unlines $
      renderUnitSection unit
    : renderServiceSection service
    : maybeToList (renderInstallSection <$> install)

renderUnitSection :: UnitSection -> Text
renderUnitSection UnitSection {..} = T.unlines $
    [ "[Unit]"
    , "Description=" <> fromMaybe mempty description
    ]
    <> map (("Wants="  <>) . unitName) wants
    <> map (("Before=" <>) . unitName) before
    <> map (("After="  <>) . unitName) after

renderServiceSection :: ServiceSection -> Text
renderServiceSection ServiceSection {..} = T.unlines . catMaybes $
      pure "[Service]"
    : map (("EnvironmentFile=" <>) . toS) environmentFile
    : map ("ExecStartPre=" <>) execStartPre
    : pure ("ExecStart=" <> execStart)
    : map ("ExecStop=" <>) execStop
    : map ("ExecStopPost=" <>) execStopPost
    : Map.foldrWithKey
        (\k v acc -> Just ("Environment=\"" <> k <> "=" <> v <> "\"") : acc)
        []
        environment

renderInstallSection :: InstallSection -> Text
renderInstallSection InstallSection {..} = T.unlines $
       pure "[Install]"
    <> map (("WantedBy="   <>) . unitName) wantedBy
    <> map (("RequiredBy=" <>) . unitName) requiredBy
    <> map (("Also="       <>) . unitName) also

unitName :: Unit -> Text
unitName = \case
    KnownUnit x -> x
    Unit    x _ -> x
