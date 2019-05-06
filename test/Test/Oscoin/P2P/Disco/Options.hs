module Test.Oscoin.P2P.Disco.Options (tests, props) where

import           Oscoin.Prelude

import           Oscoin.P2P.Disco.Options

import           Options.Applicative
                 ( ParserResult(..)
                 , briefDesc
                 , defaultPrefs
                 , execParserPure
                 , info
                 )

import           Oscoin.Test.Crypto (Dict(..), IsCrypto)
import qualified Test.Oscoin.P2P.Disco.Options.Gen as Gen

import           Hedgehog
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.P2P.Disco.Options"
    [ testProperty "prop_roundtripOptions" (prop_roundtripOptions d)
    ]

props :: Dict (IsCrypto c) -> IO Bool
props d = checkParallel $ Group "Test.Oscoin.P2P.Disco.Options"
    [ ("prop_roundtripOptions", prop_roundtripOptions d)
    ]

prop_roundtripOptions :: forall c. Dict (IsCrypto c) -> Property
prop_roundtripOptions Dict = property $ do
    dopt <- forAll Gen.genOptions
    let
        opts  = map toS $ renderDiscoOpts dopt
        pinfo = info (discoParser @c) briefDesc
     in
        case execParserPure defaultPrefs pinfo opts of
            Success dopt'           -> annotateShow opts  >> dopt' === dopt
            Failure e               -> annotateShow e     *> failure
            CompletionInvoked compl -> annotateShow compl *> failure
