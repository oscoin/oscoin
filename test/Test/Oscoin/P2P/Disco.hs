module Test.Oscoin.P2P.Disco
    ( tests
    , props
    )
where

import           Oscoin.Prelude hiding (retry)

import           Oscoin.P2P.Disco (Options(..), withDisco)
import qualified Oscoin.P2P.Disco.MDns as MDns
import           Oscoin.P2P.Types (Network, readNodeAddr)

import           Control.Retry
import qualified Data.Set as Set
import qualified Network.DNS as DNS
import           Network.Socket
                 ( AddrInfo(..)
                 , AddrInfoFlag(..)
                 , HostAddress
                 , HostAddress6
                 , PortNumber
                 , SockAddr(..)
                 , SocketType(Datagram)
                 , bind
                 , close
                 , defaultHints
                 , defaultProtocol
                 , getAddrInfo
                 , socket
                 , socketPort
                 , tupleToHostAddress
                 , tupleToHostAddress6
                 )
import           Network.Socket.ByteString (recvFrom, sendAllTo)

import           Test.Oscoin.P2P.Gen (genSomeNetwork)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Test.Oscoin.P2P.Disco"
    [ testProperty "prop_multicast"   prop_multicast
    , testProperty "prop_staticPeers" prop_staticPeers
    , testProperty "prop_dnsSD"       prop_dnsSD
    ]

props :: IO Bool
props = checkParallel $ Group "Test.Oscoin.P2P.Disco"
    [ ("prop_multicast"  , prop_multicast  )
    , ("prop_staticPeers", prop_staticPeers)
    , ("prop_dnsSD"      , prop_dnsSD      )
    ]

prop_multicast :: Property
prop_multicast = withTests 1 . property $ do
    net       <- forAll $ Gen.prune genSomeNetwork
    (a, b, c) <- evalIO $ runPropMulticast net
    annotateShow (a, b, c)
    a /== mempty
    a === b
    b === c

runPropMulticast :: Network -> IO (Set SockAddr, Set SockAddr, Set SockAddr)
runPropMulticast net =
    let
        opts = Options
             { optNetwork    = net
             , optSeeds      = []
             , optSDDomains  = []
             , optEnableMDns = True
             , optNameserver = Nothing
             }
     in
        withDisco noTracing opts (srvs 6942) $ \resolve0 ->
        withDisco noTracing opts (srvs 4269) $ \resolve1 ->
        withDisco noTracing opts (srvs 6666) $ \resolve2 ->
            (,,) <$> retry resolve0
                 <*> retry resolve1
                 <*> retry resolve2
  where
    srvs  = Set.fromList . pure . MDns.Service "gossip" MDns.TCP "127.0.0.1"
    retry = retrying (limitRetries 5 <> fullJitterBackoff 5000)
                     (const $ \x -> pure $ Set.size x < 3)
                     . const

    noTracing _ = pure ()

prop_staticPeers :: Property
prop_staticPeers = withTests 1 . property $ do
    net   <- forAll $ Gen.prune genSomeNetwork
    seeds <- for seedAddrs (either (const failure) pure . readNodeAddr)
    found <-
        evalIO . runNameserver $ \port ->
            let
                opts = Options
                     { optNetwork    = net
                     , optSeeds      = seeds
                     , optSDDomains  = []
                     , optEnableMDns = False
                     , optNameserver = Just ("127.0.0.1", port)
                     }
             in
                runDisco opts mempty
    annotateShow found

    have6 <- liftIO haveIPv6
    found === expected have6
  where
    seedAddrs :: [String]
    seedAddrs =
        zipWith (\h p -> h <> ":" <> p)
                [ "testnet.oscoin.io"
                , "oscoin.polkadot.network"
                , "blockchain.amazonaws.com"
                ]
                (map show seedPorts)

    seedPorts = [6942, 69, 666]

    expected have6 = Set.fromList $
        flip concatMap seedPorts $ \port ->
            SockAddrInet port lol : bool [] [SockAddrInet6 port 0 lol6 0] have6

prop_dnsSD :: Property
prop_dnsSD = withTests 1 . property $ do
    net   <- forAll $ Gen.prune genSomeNetwork
    found <-
        evalIO . runNameserver $ \port ->
            let
                opts = Options
                     { optNetwork    = net
                     , optSeeds      = []
                     , optSDDomains  = ["svc.cluster.local"]
                     , optEnableMDns = False
                     , optNameserver = Just ("127.0.0.1", port)
                     }
             in
                runDisco opts mempty
    annotateShow found

    have6 <- liftIO haveIPv6
    found === expected have6
  where
    expected have6 = Set.fromList $
        SockAddrInet 42 lol : bool [] [SockAddrInet6 42 0 lol6 0] have6

-- Helpers ---------------------------------------------------------------------

lol :: HostAddress
lol = tupleToHostAddress  (127, 0, 0, 1)

lol6 :: HostAddress6
lol6 = tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 1)

haveIPv6 :: IO Bool
haveIPv6 = do
    ai <-
        getAddrInfo (Just hints) (Just "::1") Nothing
            `catchIO` const (pure mempty)
    pure $ not (null ai)
  where
    hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_ALL, AI_NUMERICHOST] }


runDisco :: Options -> Set MDns.Service -> IO (Set SockAddr)
runDisco opts srvs = withDisco (const $ pure ()) opts srvs identity

runNameserver :: (PortNumber -> IO a) -> IO a
runNameserver k =
    bracket open (close . fst)    $ \(sock, addr) ->
    withAsync (forever (go sock)) $ \server -> do
        res <- k addr
        res <$ cancel server
  where
    go sock = do
        (bs, from) <- recvFrom sock 512
        for_ (DNS.decode bs) $ \msg ->
            let
                resp = DNS.defaultResponse
                    { DNS.header = (DNS.header DNS.defaultResponse)
                        { DNS.identifier = DNS.identifier (DNS.header msg) }
                    , DNS.question = DNS.question msg
                    , DNS.answer  = mapMaybe answer $ DNS.question msg
                    }
             in
                sendAllTo sock (DNS.encode resp) from

    answer (DNS.Question dom DNS.SRV) = pure DNS.ResourceRecord
        { DNS.rrname  = dom
        , DNS.rrtype  = DNS.SRV
        , DNS.rrclass = DNS.classIN
        , DNS.rrttl   = 0
        , DNS.rdata   = DNS.RD_SRV 10 10 42 "acme.acme"
        }
    answer (DNS.Question dom DNS.A)   = pure DNS.ResourceRecord
        { DNS.rrname  = dom
        , DNS.rrtype  = DNS.A
        , DNS.rrclass = DNS.classIN
        , DNS.rrttl   = 0
        , DNS.rdata   = DNS.RD_A "127.0.0.1"
        }
    answer (DNS.Question dom DNS.AAAA) = pure DNS.ResourceRecord
        { DNS.rrname  = dom
        , DNS.rrtype  = DNS.AAAA
        , DNS.rrclass = DNS.classIN
        , DNS.rrttl   = 0
        , DNS.rdata   = DNS.RD_AAAA "::1"
        }

    answer _ = Nothing

    open = do
        info:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
        sock   <- socket (addrFamily info) Datagram defaultProtocol
        bind sock $ addrAddress info
        (sock,) <$> socketPort sock
