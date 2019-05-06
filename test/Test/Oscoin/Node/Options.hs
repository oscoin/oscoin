module Test.Oscoin.Node.Options (tests, props) where

import           Oscoin.Prelude

import           Oscoin.Configuration (getConfigPaths)
import           Oscoin.Node.Options

import           Options.Applicative
                 ( ParserResult(..)
                 , briefDesc
                 , defaultPrefs
                 , execParserPure
                 , info
                 )

import           Oscoin.Test.Crypto (Dict(..), IsCrypto)
import           Test.Oscoin.Node.Options.Gen (genNodeOptions)

import           Hedgehog
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Node.Options"
    [ testProperty "prop_roundtripOptions" (prop_roundtripOptions d)
    ]

props :: Dict (IsCrypto c) -> IO Bool
props d = checkParallel $ Group "Test.Oscoin.Node.Options"
    [ ("prop_roundtripOptions", prop_roundtripOptions d)
    ]

prop_roundtripOptions :: forall c. Dict (IsCrypto c) -> Property
prop_roundtripOptions Dict = property $ do
    cps  <- liftIO getConfigPaths
    nopt <- forAll genNodeOptions
    let
        opts  = map toS $ renderNodeOptionsOpts nopt
        pinfo = info (nodeOptionsParser @c cps) briefDesc
     in
        case execParserPure defaultPrefs pinfo opts of
            Success nopt'           -> annotateShow opts  >> nopt' === nopt
            Failure e               -> annotateShow e     *> failure
            CompletionInvoked compl -> annotateShow compl *> failure

