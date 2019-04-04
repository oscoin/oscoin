module Test.Oscoin.Configuration (tests, props) where

import           Oscoin.Prelude

import           Oscoin.Configuration
                 ( readEnvironmentText
                 , readNetwork
                 , renderEnvironment
                 , renderNetwork
                 )

import           Test.Oscoin.Configuration.Gen (genNetwork)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Test.Oscoin.Configuration"
    [ testProperty "prop_roundtripEnvironment"      prop_roundtripEnvironment
    , testProperty "prop_roundtripNetworkStringly"  prop_roundtripNetworkStringly
    ]

props :: IO Bool
props = checkParallel $ Group "Test.Oscoin.Configuration"
    [ ("prop_roundtripEnvironment", prop_roundtripEnvironment)
    , ("prop_roundtripNetworkStringly" , prop_roundtripNetworkStringly )
    ]

prop_roundtripEnvironment :: Property
prop_roundtripEnvironment = property $ do
    env <- forAll Gen.enumBounded
    tripping env renderEnvironment readEnvironmentText

prop_roundtripNetworkStringly :: Property
prop_roundtripNetworkStringly = property $ do
    net <- forAll genNetwork
    tripping net renderNetwork (readNetwork . toS)
