-- | Export "Radicle" and additional Radicle functions shared
-- throughout the codebase but not available in Radicle.
module Radicle.Extended
    ( keyword
    , atom
    , list
    , listPrimop
    , fnApply
    , module Radicle
    ) where

import           Oscoin.Prelude hiding (list)
import           Radicle

keyword :: Text -> Value
keyword = Keyword . Ident

atom :: Text -> Value
atom = Atom . Ident

listPrimop :: Value
listPrimop = Primop $ Ident "list"

-- | Return a Radicle value that evaluates to the given list of Radicle
-- values.
-- @
--      Rad.eval (list vals) = pure vals
-- @
list :: [Value] -> Value
list vals = List $ listPrimop:vals

-- | Return a Radicle expression that applies @args@ to the function
-- named @fnName@.
fnApply :: Text -> [Value] -> Value
fnApply fnName args = List $ atom fnName : args
