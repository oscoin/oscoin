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
tests = testGroup "Oscoin.Test.P2P.IO"
    [ testProperty "prop_streamingHappy" prop_streamingHappy
    , testProperty "prop_framedHappy"    prop_framedHappy
    , testProperty "prop_hybridHappy"    prop_hybridHappy
    , testProperty "prop_streamingServerFramedClientFails"
                    prop_streamingServerFramedClientFails
    , testProperty "prop_framedServerStreamingClientFails"
                    prop_framedServerStreamingClientFails
    ]

-- | For GHCi use.
props :: IO Bool
props = checkParallel $ Group "P2P.IO"
    [ ("prop_streamingHappy", prop_streamingHappy)
    , ("prop_framedHappy"   , prop_framedHappy   )
    , ("prop_hybridHappy"   , prop_hybridHappy   )
    , ("prop_streamingServerFramedClientFails"
      , prop_streamingServerFramedClientFails
      )
    , ("prop_framedServerStreamingClientFails"
      , prop_framedServerStreamingClientFails
      )
    ]

prop_streamingHappy :: Property
prop_streamingHappy = property $
    forAll nonEmptyFrobs >>= compatible streamingServer streamingClient

prop_framedHappy :: Property
prop_framedHappy = property $
    forAll nonEmptyFrobs >>= compatible framedServer framedClient

prop_hybridHappy :: Property
prop_hybridHappy = property $ do
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

prop_framedServerStreamingClientFails :: Property
prop_framedServerStreamingClientFails = property $
    forAll nonEmptyFrobs >>= incompatible server client
  where
    server s    = try $ framedServer s
    client p ys = streamingClient p ys `catchIO` const (pure ()) -- Ignore server disconnecting

prop_streamingServerFramedClientFails :: Property
prop_streamingServerFramedClientFails = property $
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
