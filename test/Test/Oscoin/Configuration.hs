module Test.Oscoin.Configuration (tests, props) where

import           Oscoin.Prelude

import           Oscoin.Configuration
                 ( Network
                 , readEnvironmentText
                 , readNetworkText
                 , renderEnvironment
                 , renderNetwork
                 )

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Test.Oscoin.Configuration"
    [ testProperty "prop_roundtripEnvironment"     prop_roundtripEnvironment
    , testProperty "prop_roundtripNetworkText"     prop_roundtripNetworkText
    , testProperty "prop_roundtripNetworkShowRead" prop_roundtripNetworkShowRead
    ]

props :: IO Bool
props = checkParallel $ Group "Test.Oscoin.Configuration"
    [ ("prop_roundtripEnvironment"    , prop_roundtripEnvironment)
    , ("prop_roundtripNetworkText"    , prop_roundtripNetworkText)
    , ("prop_roundtripNetworkShowRead", prop_roundtripNetworkShowRead)
    ]

prop_roundtripEnvironment :: Property
prop_roundtripEnvironment = property $ do
    env <- forAll Gen.enumBounded
    tripping env renderEnvironment readEnvironmentText

prop_roundtripNetworkText :: Property
prop_roundtripNetworkText = property $ do
    net <- forAll Gen.enumBounded
    tripping net renderNetwork (readNetworkText . toS)

prop_roundtripNetworkShowRead :: Property
prop_roundtripNetworkShowRead = property $ do
    net <- forAll Gen.enumBounded
    tripping net (show @Network) readEither
