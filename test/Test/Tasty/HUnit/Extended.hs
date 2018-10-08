-- | Drop-in replacement for "Test.Tasty.HUnit".
--
-- * Assertion are lifted to 'MonadIO'
-- * Equality operators '@=?' and '@?=' display pretty diffs.
--
module Test.Tasty.HUnit.Extended
    ( HUnit.testCase
    , HUnit.testCaseInfo
    , HUnit.testCaseSteps

    , HUnit.Assertion
    , assertFailure
    , assertBool
    , assertEqual
    , (@?=)
    , (@=?)
    ) where

import           Oscoin.Prelude

import qualified Data.Algorithm.Diff as Diff
import           Data.Text as T
import qualified Test.Tasty.HUnit as HUnit
import           Text.Nicify (nicify)

assertFailure :: (MonadIO m, HasCallStack) => String -> m a
assertFailure msg = liftIO $ HUnit.assertFailure msg

assertBool :: (MonadIO m, HasCallStack) => String -> Bool -> m ()
assertBool msg condition = liftIO $ HUnit.assertBool msg condition

assertEqual :: (MonadIO m, HasCallStack, Eq a, Show a) => String -> a -> a -> m ()
assertEqual msg expected actual = liftIO $ HUnit.assertEqual msg expected actual

infix 1 @?
(@?) :: (MonadIO m, HUnit.AssertionPredicable t, HasCallStack) => t -> String -> m ()
(@?) predi msg = liftIO $ (HUnit.@?) predi msg

infix 1 @?=
(@?=) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(@?=) have want = have == want @? T.unpack (prettyDiff have want)

infix 1 @=?
(@=?) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(@=?) = flip (@?=)

prettyDiff :: Show a => a -> a -> Text
prettyDiff have want = T.unlines $ addSign <$> Diff.getDiff (pp have) (pp want)
  where
    pp = T.lines . T.pack . nicify . show
    addSign (Diff.Both _ s) = "        " <> s
    addSign (Diff.First  s) = "have -> " <> s
    addSign (Diff.Second s) = "want -> " <> s
