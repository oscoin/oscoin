module Test.Oscoin.Configuration (tests, props) where

import           Oscoin.Prelude

import           Oscoin.Configuration
                 ( Network
                 , getConfigPaths
                 , pathsParser
                 , readNetworkText
                 , renderNetwork
                 , renderPathsOpts
                 )


import qualified Test.Oscoin.Configuration.Gen as Gen

import           Options.Applicative
                 ( ParserResult(..)
                 , briefDesc
                 , defaultPrefs
                 , execParserPure
                 , info
                 )

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Test.Oscoin.Configuration"
    [ testProperty "prop_roundtripNetworkText"     prop_roundtripNetworkText
    , testProperty "prop_roundtripNetworkShowRead" prop_roundtripNetworkShowRead
    , testProperty "prop_roundtripPathsCLI"        prop_roundtripPathsCLI
    ]

props :: IO Bool
props = checkParallel $ Group "Test.Oscoin.Configuration"
    [ ("prop_roundtripNetworkText"    , prop_roundtripNetworkText)
    , ("prop_roundtripNetworkShowRead", prop_roundtripNetworkShowRead)
    , ("prop_roundtripPathsCLI"       , prop_roundtripPathsCLI)
    ]

prop_roundtripNetworkText :: Property
prop_roundtripNetworkText = property $ do
    net <- forAll Gen.enumBounded
    tripping net renderNetwork (readNetworkText . toS)

prop_roundtripNetworkShowRead :: Property
prop_roundtripNetworkShowRead = property $ do
    net <- forAll Gen.enumBounded
    tripping net (show @Network) readEither

prop_roundtripPathsCLI :: Property
prop_roundtripPathsCLI = property $ do
    cps <- liftIO getConfigPaths
    ps  <- forAll Gen.paths
    let
        opts  = map toS $ renderPathsOpts ps
        pinfo = info (pathsParser cps) briefDesc
     in
        case execParserPure defaultPrefs pinfo opts of
            Success ps'             -> annotateShow opts  >> ps' === ps
            Failure e               -> annotateShow e     *> failure
            CompletionInvoked compl -> annotateShow compl *> failure
