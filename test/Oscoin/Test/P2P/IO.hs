module Oscoin.Test.P2P.IO (tests, props) where

import           Oscoin.Prelude

import qualified Oscoin.P2P.Transport as Transport

import           Oscoin.Test.P2P.Helpers

import           Codec.Serialise (Serialise)
import           Control.Applicative (liftA3)
import           Control.Concurrent.Async (concurrently)
import           Data.List.NonEmpty (nonEmpty)
import           Network.Socket (Socket)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "IO"
    [ testGroup "Wire"
        [ testProperty "Works: Streaming Server <-> Streaming Client"
                        propStreamingHappy
        , testProperty "Works: Framed Server <-> Framed Client"
                        propFramedHappy
        , testProperty "Works: Hybrid Server <-> Hybrid Client"
                        propHybridHappy
        , testProperty "Fails: Streaming Server <-> Framed Client"
                        propStreamingServerFramedClient
        , testProperty "Fails: Framed Server <-> Streaming Client"
                        propFramedServerStreamingClient
        ]
    ]

-- | For GHCi use.
props :: IO Bool
props = checkParallel $ Group "P2P.IO"
    [ ("prop_streaming_happy",  propStreamingHappy)
    , ("prop_framed_happy",     propFramedHappy)
    , ("prop_hybrid_happy",     propHybridHappy)
    , ("prop_streaming_framed", propStreamingServerFramedClient)
    , ("prop_framed_streaming", propFramedServerStreamingClient)
    ]

propStreamingHappy :: Property
propStreamingHappy = property $
    forAll nonEmptyFrobs >>= compatible streamingServer streamingClient

propFramedHappy :: Property
propFramedHappy = property $
    forAll nonEmptyFrobs >>= compatible framedServer framedClient

propHybridHappy :: Property
propHybridHappy = property $ do
    framed    <- forAll nonEmptyFrobs
    streaming <- forAll nonEmptyFrobs

    annotate $
           show (length framed) <> " framed, "
        <> show (length streaming) <> " streaming"

    xs' <-
        map fst . evalIO . bind $ \(port, sock) ->
            concurrently (hybridServer (length framed) sock) $
                hybridClient port framed streaming

    Just (framed <> streaming) === nonEmpty xs'

propFramedServerStreamingClient :: Property
propFramedServerStreamingClient = property $
    forAll nonEmptyFrobs >>= incompatible server client
  where
    server s    = try $ framedServer s
    client p ys = streamingClient p ys `catchIO` const (pure ()) -- Ignore server disconnecting

propStreamingServerFramedClient :: Property
propStreamingServerFramedClient = property $
    forAll nonEmptyFrobs >>= incompatible server client
  where
    server s    = try $ streamingServer s
    client p ys = framedClient p ys `catchIO` const (pure ()) -- Ignore server disconnecting

type Server a = Socket -> IO a
type Client a = Int -> NonEmpty a -> IO ()

compatible
    :: (Eq a, Show a)
    => Server   [a]
    -> Client    a
    -> NonEmpty  a
    -> PropertyT IO ()
compatible server client xs = do
    xs' <-
        map fst . evalIO . bind $ \(port, sock) ->
            concurrently (server sock) (client port xs)

    Just xs === nonEmpty xs'

incompatible
    :: Show a
    => Server (Either Transport.RecvError [a])
    -> Client   a
    -> NonEmpty a
    -> PropertyT IO ()
incompatible server client xs = do
    xs' <-
        map fst . evalIO . bind $ \(port, sock) ->
            concurrently (server sock) (client port xs)

    annotateShow xs'
    assert $ isGarbage xs'
  where
    isGarbage (Left Transport.RecvGarbage{}) = True
    isGarbage _                              = False

--------------------------------------------------------------------------------

data Frob = Frob
    { frobFoo :: Word
    , frobBoo :: Text
    , frobLoo :: Char
    } deriving (Eq, Show, Generic)

instance Serialise Frob

genFrob :: GenT Identity Frob
genFrob = liftA3 Frob foo boo loo
  where
    foo = Gen.prune $ Gen.word Range.constantBounded
    boo = Gen.prune $ Gen.text (Range.linear 0 255) Gen.unicode
    loo = Gen.prune $ Gen.unicode

nonEmptyFrobs :: GenT Identity (NonEmpty Frob)
nonEmptyFrobs = Gen.nonEmpty (Range.linear 0 100) genFrob
