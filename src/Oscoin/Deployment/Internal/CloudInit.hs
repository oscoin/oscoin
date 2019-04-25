{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Oscoin.Deployment.Internal.CloudInit
    ( CloudConfig
    , mkCloudConfig
    , renderCloudConfig

    , User
    , mkUser
    , rootUser
    , renderUidOrName

    , Group
    , mkGroup
    , rootGroup
    , renderGidOrName

    , Owner
    , mkOwner
    , rootOwner

    , WriteFiles
    , mkWriteFiles

    , FileContents
    , plainTextFileContents
    , base64FileContents

    , FileMode
    , fromPosixFileMode
    )
where

import           Oscoin.Prelude hiding (group)

import           Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson.Types as Aeson
import           Data.ByteString.BaseN (AtBase, encodedText)
import           Data.Generics.Product
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Yaml as Yaml
import           Lens.Micro (set)
import           Lens.Micro.Extras (view)
import           Numeric (showIntAtBase)
import qualified System.Posix as Posix

data CloudConfig = CloudConfig
    { users       :: Vector User
    , groups      :: Vector Group
    , write_files :: Vector WriteFiles
    , runcmd      :: Vector Text
    , bootcmd     :: Vector Text
   } deriving Generic

instance ToJSON CloudConfig where
    toJSON cc = Aeson.object
        [ "users"       .= users cc
        , "groups"      .= map (view (the @"name")) (view (the @"groups") cc)
        , "write_files" .= write_files cc
        , "runcmd"      .= runcmd cc
        , "bootcmd"     .= bootcmd cc
        ]

    toEncoding cc = Aeson.pairs $
           "users"       .= users cc
        <> "groups"      .= map (view (the @"name")) (view (the @"groups") cc)
        <> "write_files" .= write_files cc
        <> "runcmd"      .= runcmd cc
        <> "bootcmd"     .= bootcmd cc

instance Semigroup CloudConfig where
    a <> b = CloudConfig
        { users       = on (<>) users a b
        , groups      = on (<>) (groups :: CloudConfig -> Vector Group) a b
        , write_files = on (<>) write_files a b
        , runcmd      = on (<>) runcmd a b
        , bootcmd     = on (<>) bootcmd a b
        }

instance Monoid CloudConfig where
    mempty  = CloudConfig mempty mempty mempty mempty mempty
    mappend = (<>)

mkCloudConfig :: CloudConfig
mkCloudConfig = mempty

renderCloudConfig :: CloudConfig -> Text
renderCloudConfig cc = "#cloud-config\n\n" <> toS (Yaml.encode cc)

data User = User
    { name   :: Text
    , uid    :: Maybe Word16
    , groups :: Set Group
    } deriving (Eq, Ord, Generic)

instance ToJSON User where
    toJSON u = Aeson.object $ catMaybes
        [ Just $ "name" .= view (the @"name") u
        , ("uid" .=) . toJSON <$> uid u
        , if Set.null (view (the @"groups") u) then
            Nothing
          else
            Just
                . ("groups" .=)
                . toJSON
                . mconcat
                . intersperse ", "
                . Set.toList
                . Set.map (view (the @"name"))
                $ view (the @"groups") u
        ]

    toEncoding u = Aeson.pairs . mconcat $ catMaybes
        [ Just $ "name" .= view (the @"name") u
        , ("uid" .=) <$> uid u
        , if Set.null (view (the @"groups") u) then
            Nothing
          else
            Just
                . ("groups" .=)
                . mconcat
                . intersperse ", "
                . Set.toList
                . Set.map (view (the @"name"))
                $ view (the @"groups") u
        ]

mkUser :: Text -> User
mkUser n = User { name = n, uid = Nothing, groups = mempty }

rootUser :: User
rootUser = mkUser "root" & set (the @"uid") (Just 0)

renderUidOrName :: User -> Text
renderUidOrName u = maybe (view (the @"name") u) show $ view (the @"uid") u

data Group = Group
    { name :: Text
    , gid  :: Maybe Word16
    } deriving (Eq, Ord, Generic)

instance ToJSON Group where
    toJSON     = Aeson.genericToJSON     jsonOpts
    toEncoding = Aeson.genericToEncoding jsonOpts

mkGroup :: Text -> Group
mkGroup n = Group { name = n, gid = Nothing }

rootGroup :: Group
rootGroup = mkGroup "root" & set (the @"gid") (Just 0)

renderGidOrName :: Group -> Text
renderGidOrName g = maybe (view (the @"name") g) show $ view (the @"gid") g

data Owner = Owner
    { user  :: User
    , group :: Maybe Group
    } deriving Generic

renderOwner :: Owner -> Text
renderOwner (Owner user group) =
    mconcat . intersperse ":" $ catMaybes
        [ Just $ view (the @"name") user
        , view (the @"name") <$> group
        ]

instance ToJSON Owner where
    toJSON     = toJSON     . renderOwner
    toEncoding = toEncoding . renderOwner

mkOwner :: User -> Owner
mkOwner u = Owner { user = u, group = Nothing }

rootOwner :: Owner
rootOwner = mkOwner rootUser & set (the @"group") (Just rootGroup)

data WriteFiles = WriteFiles
    { path        :: FilePath
    , permissions :: FileMode
    , owner       :: Owner
    , content     :: FileContents
    , append      :: Maybe Bool
    } deriving Generic

instance ToJSON WriteFiles where
    toJSON     = Aeson.genericToJSON     jsonOpts
    toEncoding = Aeson.genericToEncoding jsonOpts

mkWriteFiles
    :: FilePath
    -> FileMode
    -> Owner
    -> FileContents
    -> WriteFiles
mkWriteFiles fp perm own cnt = WriteFiles
    { path        = fp
    , permissions = perm
    , owner       = own
    , content     = cnt
    , append      = Nothing
    }

newtype FileMode = FileMode Posix.FileMode
    deriving Generic

fromPosixFileMode :: Posix.FileMode -> FileMode
fromPosixFileMode = FileMode

fileModeOctal :: FileMode -> String
fileModeOctal (FileMode cmode) =
    showIntAtBase 8 (toEnum . (+ fromEnum '0')) cmode mempty

instance ToJSON FileMode where
    toJSON     = toJSON     . fileModeOctal
    toEncoding = toEncoding . fileModeOctal

data FileContents
    = Plain  Text
    | Base64 (AtBase "64")
    deriving Generic

instance ToJSON FileContents where
    toJSON (Plain  txt) = toJSON txt
    toJSON (Base64 b64) = toJSON $ encodedText b64

    toEncoding (Plain  txt) = toEncoding txt
    toEncoding (Base64 b64) = toEncoding $ encodedText b64

plainTextFileContents :: Text -> FileContents
plainTextFileContents = Plain

base64FileContents :: AtBase "64" -> FileContents
base64FileContents = Base64

jsonOpts :: Aeson.Options
jsonOpts = Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    , Aeson.sumEncoding       = Aeson.UntaggedValue
    }
