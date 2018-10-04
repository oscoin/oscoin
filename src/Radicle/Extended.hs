-- | Export "Radicle" and additional Radicle functions shared
-- throughout the codebase but not available in Radicle.
module Radicle.Extended
    ( keyword
    , atom
    , fnApply

    , Rad.tagDefault
    , Rad.annotate
    , module Radicle
    ) where

import           Oscoin.Prelude

import           Radicle
import qualified Radicle.Internal.Annotation as Rad (annotate, tagDefault)

keyword :: Text -> Value
keyword = Keyword . unsafeToIdent

atom :: Text -> Value
atom = Atom . unsafeToIdent

-- | Return a Radicle expression that applies @args@ to the function
-- named @fnName@.
fnApply :: Text -> [Value] -> Value
fnApply fnName args = List $ atom fnName : args
