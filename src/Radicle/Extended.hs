-- | Export "Radicle" and additional Radicle functions shared
-- throughout the codebase but not available in Radicle.
module Radicle.Extended
    ( keyword
    , atom
    , fnApply
    , prettyValue
    , parseFromJson

    , Rad.tagDefault
    , Rad.annotate

    , module Radicle
    ) where

import           Oscoin.Prelude

import           Control.Monad.Fail
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Radicle hiding (Env, pureEnv)
import qualified Radicle.Internal.Annotation as Rad (annotate, tagDefault)

keyword :: Text -> Value
keyword = Keyword . unsafeToIdent

atom :: Text -> Value
atom = Atom . unsafeToIdent

-- | Return a Radicle expression that applies @args@ to the function
-- named @fnName@.
fnApply :: Text -> [Value] -> Value
fnApply fnName args = List $ atom fnName : args

-- | Pretty a Radicle 'Value' with default settings.
prettyValue :: Value -> Text
prettyValue =
    renderStrict . layoutSmart layoutOptions . pretty
  where
    layoutOptions =
        LayoutOptions {layoutPageWidth = AvailablePerLine 76 1.0}

-- | Parse a radicle value from JSON where the radicle value is
-- represented as a string of Radicle code.
parseFromJson :: Aeson.Value -> Aeson.Parser Value
parseFromJson =
    Aeson.withText "Radicle code" $ \c ->
        case parse "FromJSON" c of
            Left err  -> fail $ "error parsing Value: " <> toS err
            -- Nb. We are currently working with the default `Radicle.Value` type, which is
            -- tagged (annotated) - but we are not using the tags, since they yield different
            -- values and thus different encodings/hashes. Therefore, we untag everything
            -- and re-tag to ensure no tags are preserved from the source, while still returning
            -- a tagged value.
            Right val -> pure $ Rad.tagDefault $ untag val
