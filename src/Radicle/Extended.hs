-- | Export "Radicle" and additional Radicle functions shared
-- throughout the codebase but not available in Radicle.
module Radicle.Extended
    ( keyword
    , atom
    , fnApply
    , prettyValue

    , Rad.tagDefault
    , Rad.annotate

    , module Radicle
    ) where

import           Oscoin.Prelude hiding (list)
import           Data.Text.Prettyprint.Doc hiding (list)
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
