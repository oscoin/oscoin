module Test.QuickCheck.Extended
    ( (===)
    ) where

import           Prelude

import qualified Data.Text as T
import           GHC.Stack

import           Oscoin.Test.Util

import           Test.QuickCheck hiding ((===))
import qualified Test.QuickCheck as QC

-- | Drop-in for QuickCheck '===' that relies on the 'Condensed' instance to
-- show the counterexample.
(===) :: forall b. (HasCallStack, Eq b, Show b, Condensed b) => b -> b -> Property
(===) actual expected =
    withFrozenCallStack $
        counterexample (mismatch callStack) (actual QC.=== expected)
  where
    mismatch :: CallStack -> String
    mismatch cs =
        let calledAt = case getCallStack cs of
                         [(_, loc)] -> srcLocFile loc <> ":" <>
                                       show (srcLocStartLine loc) <> ":" <>
                                       show (srcLocStartCol loc)
                         _          -> "(unknown)"
        in unlines [
                 "api call at " <> calledAt <> " yielded a result mismatch!\n"
               , "expected = " <> T.unpack (condensed expected) <> "\n"
               , "actual   = " <> T.unpack (condensed actual)   <> "\n"
               , "Counterexample is:"
               ]
