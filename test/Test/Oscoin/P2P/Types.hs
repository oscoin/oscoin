module Test.Oscoin.P2P.Types (tests , props) where

import           Oscoin.Prelude

import           Oscoin.P2P.Types
                 ( MsgId
                 , NodeAddr(..)
                 , domainToHostname
                 , hostToEither
                 , hostnameToDomain
                 , randomNetwork
                 , readHost
                 , readHostnameText
                 , readNetworkText
                 , readNodeAddr
                 , renderHost
                 , renderHostname
                 , renderNetwork
                 , showNodeAddr
                 )

import           Codec.Serialise (deserialiseOrFail, serialise)
import           Data.IP (IP(IPv6))
import           System.Random.SplitMix (mkSMGen)

import           Oscoin.Test.Crypto
import           Test.Oscoin.P2P.Gen
                 ( genHost
                 , genHostname
                 , genMsg
                 , genMsgId
                 , genNetwork
                 , genPortNumber
                 )

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.P2P.Types"
    [ testProperty "prop_roundtripNetworkStringly"  prop_roundtripNetworkStringly
    , testProperty "prop_randomNetworkValid"        prop_randomNetworkValid
    , testProperty "prop_roundtripNetworkSerialise" prop_roundtripNetworkSerialise
    , testProperty "prop_roundtripHostStringly"     prop_roundtripHostStringly
    , testProperty "prop_roundtripHostnameStringly" prop_roundtripHostnameStringly
    , testProperty "prop_roundtripHostnameDomain"   prop_roundtripHostnameDomain
    , testProperty "prop_readNodeAddr"              (prop_readNodeAddr d)
    , testProperty "prop_roundtripShowReadNodeAddr" (prop_roundtripShowReadNodeAddr d)
    , testProperty "prop_roundtripMsgSerialise"     (prop_roundtripMsgSerialise d)
    , testProperty "prop_roundtripMsgIdSerialise"   (prop_roundtripMsgIdSerialise d)
    ]

props :: Dict (IsCrypto c) -> IO Bool
props d = checkParallel $ Group "Test.Oscoin.P2P.Types"
    [ ("prop_roundtripNetworkStringly" , prop_roundtripNetworkStringly   )
    , ("prop_randomNetworkValid"       , prop_randomNetworkValid         )
    , ("prop_roundtripNetworkSerialise", prop_roundtripNetworkSerialise  )
    , ("prop_roundtripHostStringly"    , prop_roundtripHostStringly      )
    , ("prop_roundtripHostnameStringly", prop_roundtripHostnameStringly  )
    , ("prop_roundtripHostnameDomain"  , prop_roundtripHostnameDomain    )
    , ("prop_readNodeAddr"             , prop_readNodeAddr d             )
    , ("prop_roundtripShowReadNodeAddr", prop_roundtripShowReadNodeAddr d)
    , ("prop_roundtripMsgSerialise"    , prop_roundtripMsgSerialise d    )
    , ("prop_roundtripMsgIdSerialise"  , prop_roundtripMsgIdSerialise d  )
    ]

prop_roundtripNetworkStringly :: Property
prop_roundtripNetworkStringly = property $ do
    net <- forAll genNetwork
    tripping net renderNetwork readNetworkText

prop_randomNetworkValid :: Property
prop_randomNetworkValid = property $ do
    rng <- mkSMGen <$> forAll (Gen.word64 Range.linearBounded)
    tripping (randomNetwork rng) renderNetwork readNetworkText

prop_roundtripNetworkSerialise :: Property
prop_roundtripNetworkSerialise = property $ do
    net <- forAll genNetwork
    tripping net serialise deserialiseOrFail

prop_roundtripHostStringly :: Property
prop_roundtripHostStringly = property $ do
    host <- forAll genHost
    tripping host renderHost (readHost . toS)

prop_roundtripHostnameStringly :: Property
prop_roundtripHostnameStringly = property $ do
    name <- forAll genHostname
    tripping name renderHostname readHostnameText

prop_roundtripHostnameDomain :: Property
prop_roundtripHostnameDomain = property $ do
    name <- forAll genHostname
    tripping name hostnameToDomain domainToHostname

prop_readNodeAddr :: forall c. Dict (IsCrypto c) -> Property
prop_readNodeAddr Dict = property $ do
    host <- forAll genHost
    port <- forAll genPortNumber
    case hostToEither host of
        Left ip@IPv6{} ->
            let
                valid   = '[' : show ip <> "]:" <> show port
                invalid = show ip <> ":" <> show port
             in do
                readNodeAddr @c valid === Right (NodeAddr Nothing host port)
                assert $ isLeft (readNodeAddr @c invalid)

        _ ->
            let
                valid = toS (renderHost host) <> ":" <> show port
             in
                readNodeAddr @c valid === Right (NodeAddr Nothing host port)

prop_roundtripShowReadNodeAddr :: forall c. Dict (IsCrypto c) -> Property
prop_roundtripShowReadNodeAddr Dict = property $ do
    addr <- forAll $ NodeAddr Nothing <$> genHost <*> genPortNumber
    tripping addr showNodeAddr (readNodeAddr @c)

prop_roundtripMsgSerialise :: forall c. Dict (IsCrypto c) -> Property
prop_roundtripMsgSerialise Dict = property $ do
    msg <- forAll $ genMsg @c
    tripping msg serialise deserialiseOrFail

prop_roundtripMsgIdSerialise :: forall c. Dict (IsCrypto c) -> Property
prop_roundtripMsgIdSerialise Dict = property $ do
    mid <- forAll (genMsgId :: Gen (MsgId c Text))
    tripping mid serialise deserialiseOrFail
