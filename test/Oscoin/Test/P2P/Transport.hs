module Oscoin.Test.P2P.Transport (tests, props) where

import           Oscoin.Prelude

import qualified Oscoin.P2P.Transport as Transport

import           Oscoin.Test.P2P.Helpers

import           Codec.Serialise (Serialise)
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Combinators as Conduit

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Transport"
    [ testProperty "Layered framed"    propFramed
    , testProperty "Layered streaming" propStreaming
    ]

-- | For GHCi use.
props :: IO Bool
props = checkParallel $ Group "P2P.Transport"
    [ ("prop_framed",    propFramed)
    , ("prop_streaming", propStreaming)
    ]

propFramed :: Property
propFramed = property $ do
    bs <- forAll genBytes
    nl <- forAll $ Gen.int (Range.exponential 0 23)
    rs <-
        liftIO $ do
            ((write, close1), (read, close2)) <- framedPair
            let
                layers base = foldr ($) base (replicate nl mkLayer)
                tt  = layers write
                tt' = layers read
             in do
                Transport.framedSend tt  (Pide bs) `finally` close1
                Transport.framedRecv tt'           `finally` close2
    map filling rs === Right bs
  where
    mkLayer = Transport.framedEnvelope (pure . Pide) (pure . filling)

propStreaming :: Property
propStreaming = property $ do
    bs <- forAll . map toList $ Gen.nonEmpty (Range.constant 1 42) genBytes
    rs <-
        liftIO $ do
            ((write, close1), (read, close2)) <- streamingPair
            let
                tt  = mkLayer . mkLayer $ write
                tt' = mkLayer . mkLayer $ read
             in
                map snd $
                    concurrently (publish bs tt close1) (consume tt' close2)
    map filling rs === bs
  where
    mkLayer = Transport.streamingEnvelope (pure . Pide) (pure . filling)

    publish bs tt close =
        for_ bs (Transport.streamingSend tt . Pide) `finally` close

    consume tt close =
        runConduit (Transport.streamingRecv tt .| Conduit.sinkList)
            `finally` close

--------------------------------------------------------------------------------

-- | A pide is kind of like a burrito, but noone knows what the filling consists
-- of. In some cases, the customer may choose the filling, though.
newtype Pide a = Pide { filling :: a }
    deriving (Eq, Generic)

instance Serialise a => Serialise (Pide a)

genBytes :: GenT Identity LByteString
genBytes = LBS.fromStrict <$> Gen.bytes (Range.constant 1 255)
