module Oscoin.Test.P2P.Gossip.IO
    ( tests
    ) where

import           Oscoin.Prelude hiding (bracket, try)

import           Oscoin.P2P.Gossip.Connection

import           Codec.Serialise (Serialise)
import           Control.Applicative (liftA3)
import           Control.Concurrent.Async (concurrently)
import           Control.Exception.Safe (bracket, catchIO, throwM, try)
import           Data.Conduit (runConduit, (.|))
import           Data.Conduit.Combinators (sinkList, yieldMany)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Streaming.Network
import           Network.Socket (SockAddr, Socket)
import qualified Network.Socket as Sock

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "IO"
    [ testGroup "Transport"
        [ testGroup "Pure"
            [ testProperty "Streaming: deserialise . serialise == id" . property $
                propStreamingSerde =<< forAll nonEmptyFrobs
            ]
        , testGroup "Network I/O"
            [ testProperty "Works: Streaming Server <-> Streaming Client" . property $
                propNetworkStreamingHappy =<< forAll nonEmptyFrobs

            , testProperty "Works: Framed Server <-> Framed Client" . property $
                propNetworkFramedHappy =<< forAll nonEmptyFrobs

            , testProperty "Fails: Streaming Server <-> Framed Client" . property $
                propNetworkStreamingServerFramedClient =<< forAll nonEmptyFrobs

            , testProperty "Fails: Framed Server <-> Streaming Client" . property $
                propNetworkFramedServerStreamingClient =<< forAll nonEmptyFrobs
            ]
        ]
    ]
  where
    nonEmptyFrobs = Gen.nonEmpty (Range.linear 0 100) genFrob

propStreamingSerde
    :: (Serialise a, Eq a, Show a)
    => NonEmpty a
    -> PropertyT IO ()
propStreamingSerde xs = do
    xs' <-
        runConduit $
               yieldMany xs
            .| conduitEncodeCBOR
            .| conduitDecodeCBOR
            .| sinkList

    Just xs === nonEmpty xs'

propNetworkStreamingHappy
    :: (Serialise a, Eq a, Show a)
    => NonEmpty a
    -> PropertyT IO ()
propNetworkStreamingHappy xs =
    propCompatible xs streamingServer streamingClient

propNetworkFramedHappy
    :: (Serialise a, Eq a, Show a)
    => NonEmpty a
    -> PropertyT IO ()
propNetworkFramedHappy xs =
    propCompatible xs framedServer framedClient

propNetworkFramedServerStreamingClient
    :: (Serialise a, Show a)
    => NonEmpty a
    -> PropertyT IO ()
propNetworkFramedServerStreamingClient xs =
    propIncompatible xs server client
  where
    server s    = try $ framedServer s
    client p ys = streamingClient p ys `catchIO` const (pure ()) -- Ignore server disconnecting

propNetworkStreamingServerFramedClient
    :: (Serialise a, Show a)
    => NonEmpty a
    -> PropertyT IO ()
propNetworkStreamingServerFramedClient xs =
    propIncompatible xs server client
  where
    server s    = try $ streamingServer s
    client p ys = framedClient p ys `catchIO` const (pure ()) -- Ignore server disconnecting

type Server a = Socket -> IO a
type Client a = Int -> NonEmpty a -> IO ()

propCompatible
    :: (Eq a, Show a)
    => NonEmpty  a
    -> Server   [a]
    -> Client    a
    -> PropertyT IO ()
propCompatible xs server client = do
    xs' <-
        map fst . evalIO . bind $ \(port, sock) ->
            concurrently (server sock) (client port xs)

    Just xs === nonEmpty xs'

propIncompatible
    :: Show a
    => NonEmpty a
    -> Server (Either RecvError [a])
    -> Client   a
    -> PropertyT IO ()
propIncompatible xs server client = do
    xs' <-
        map fst . evalIO . bind $ \(port, sock) ->
            concurrently (server sock) (client port xs)

    annotateShow xs'
    assert $ isGarbage xs'
  where
    isGarbage (Left RecvGarbage{}) = True
    isGarbage _                    = False

--------------------------------------------------------------------------------

data Frob = Frob
    { frobFoo :: Word
    , frobBoo :: Text
    , frobLoo :: Char
    } deriving (Eq, Show, Generic)

instance Serialise Frob

genFrob :: MonadGen m => m Frob
genFrob = liftA3 Frob foo boo loo
  where
    foo = Gen.prune $ Gen.word Range.constantBounded
    boo = Gen.prune $ Gen.text (Range.linear 0 255) Gen.unicode
    loo = Gen.prune $ Gen.unicode

--------------------------------------------------------------------------------

streamingServer :: Serialise a => Socket -> IO [a]
streamingServer sock =
    accept sock $ \(sock',_) ->
        runConduit $ sockRecvStream sock' .| sinkList

streamingClient :: (Foldable t, Serialise a) => Int -> t a -> IO ()
streamingClient port msgs =
    connect port $ \(sock,_) ->
        traverse_ (sockSendStream sock) msgs

framedServer :: Serialise a => Socket -> IO [a]
framedServer sock = accept sock $ map reverse . loop [] . fst
  where
    loop acc sock' = do
        recv'd <- sockRecvFramed sock'
        case recv'd of
            Left  RecvConnReset -> pure acc
            Left  e             -> throwM e
            Right frame         -> loop (frame : acc) sock'

framedClient :: (Foldable t, Serialise a) => Int -> t a -> IO ()
framedClient port msgs =
    connect port $ \(sock,_) ->
        traverse_ (sockSendFramed sock) msgs

bind :: ((Int, Sock.Socket) -> IO a) -> IO a
bind = bracket (bindRandomPortTCP "127.0.0.1") (Sock.close . snd)

accept :: Socket -> ((Socket, SockAddr) -> IO a) -> IO a
accept sock = bracket (acceptSafe sock) (Sock.close . fst)

connect :: Int -> ((Socket, SockAddr) -> IO a) -> IO a
connect port = bracket (getSocketTCP "127.0.0.1" port) (Sock.close . fst)
