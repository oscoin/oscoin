module Integration.Tests
    ( tests
    ) where

import           Test.Tasty

import qualified Integration.Test.Executable as Executable

tests :: TestTree
tests = testGroup "oscoin"
    [ testGroup "oscoin executable" Executable.tests
    ]
