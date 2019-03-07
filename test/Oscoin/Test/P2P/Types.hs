module Oscoin.Test.P2P.Types
    ( tests
    , props
    )
where

import           Oscoin.Prelude

import           Oscoin.P2P.Types
                 ( MsgId
                 , NodeAddr(..)
                 , domainToHostname
                 , hostToEither
                 , hostnameToDomain
                 , readHost
                 , readHostnameText
                 , readNetwork
                 , readNodeAddr
                 , renderHost
                 , renderHostname
                 , renderNetwork
                 )

import           Codec.Serialise (deserialiseOrFail, serialise)
import           Data.IP (IP(IPv6))

import           Oscoin.Test.Crypto
import           Oscoin.Test.P2P.Gen
                 (genHost, genHostname, genMsg, genMsgId, genNetwork)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Oscoin.Test.P2P.Types"
    [ testProperty "prop_roundtripNetworkStringly"  prop_roundtripNetworkStringly
    , testProperty "prop_roundtripHostStringly"     prop_roundtripHostStringly
    , testProperty "prop_roundtripHostnameStringly" prop_roundtripHostnameStringly
    , testProperty "prop_roundtripHostnameDomain"   prop_roundtripHostnameDomain
    , testProperty "prop_readNodeAddr"              (prop_readNodeAddr d)
    , testProperty "prop_roundtripMsgSerialise"     (prop_roundtripMsgSerialise d)
    , testProperty "prop_roundtripMsgIdSerialise"   (prop_roundtripMsgIdSerialise d)
    ]

props :: Dict (IsCrypto c) -> IO Bool
props d = checkParallel $ Group "Oscoin.Test.P2P.Types"
    [ ("prop_roundtripNetworkStringly" , prop_roundtripNetworkStringly )
    , ("prop_roundtripHostStringly"    , prop_roundtripHostStringly    )
    , ("prop_roundtripHostnameStringly", prop_roundtripHostnameStringly)
    , ("prop_roundtripHostnameDomain"  , prop_roundtripHostnameDomain  )
    , ("prop_readNodeAddr"             , prop_readNodeAddr d           )
    , ("prop_roundtripMsgSerialise"    , prop_roundtripMsgSerialise d  )
    , ("prop_roundtripMsgIdSerialise"  , prop_roundtripMsgIdSerialise d)
    ]

prop_roundtripNetworkStringly :: Property
prop_roundtripNetworkStringly = property $ do
    net <- forAll genNetwork
    tripping net renderNetwork (readNetwork . toS)

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
    port <- forAll $ fromIntegral <$> Gen.word16 Range.constantBounded
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

prop_roundtripMsgSerialise :: forall c. Dict (IsCrypto c) -> Property
prop_roundtripMsgSerialise Dict = property $ do
    msg <- forAll $ genMsg @c
    tripping msg serialise deserialiseOrFail

prop_roundtripMsgIdSerialise :: forall c. Dict (IsCrypto c) -> Property
prop_roundtripMsgIdSerialise Dict = property $ do
    mid <- forAll (genMsgId :: Gen (MsgId c Text))
    tripping mid serialise deserialiseOrFail
