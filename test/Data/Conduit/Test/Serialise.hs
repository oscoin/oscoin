module Data.Conduit.Test.Serialise (tests) where

import           Prelude

import           Data.Conduit.Serialise (conduitDecodeCBOR, conduitEncodeCBOR)

import           Codec.Serialise (Serialise)
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit (runConduit, (.|))
import           Data.Conduit.Combinators (sinkList, yieldMany)
import qualified Data.Conduit.Combinators as Conduit
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Serialise"
    [ testProperty "conduitDecodeCBOR . conduitEncodeCBOR == id" . property $
        propSerdeIdentity =<< forAll nonEmptyWiggles
    ]

propSerdeIdentity :: (Serialise a, Eq a, Show a) => NonEmpty a -> PropertyT IO ()
propSerdeIdentity xs = do
    xs' <-
        runConduit $
               yieldMany xs
            .| conduitEncodeCBOR
            .| Conduit.concatMap LBS.toChunks
            .| conduitDecodeCBOR
            .| sinkList

    Just xs === nonEmpty xs'

--------------------------------------------------------------------------------

nonEmptyWiggles :: MonadGen m => m (NonEmpty Wiggle)
nonEmptyWiggles = Gen.nonEmpty (Range.linear 0 100) genWiggle

data Wiggle = Wiggle
    { wiggleThis  :: Word
    , wiggleThat  :: Text
    , wiggleThose :: Char
    , wiggleThese :: [Int]
    } deriving (Eq, Show, Generic)

instance Serialise Wiggle

genWiggle :: MonadGen m => m Wiggle
genWiggle = Wiggle
    <$> Gen.prune this
    <*> Gen.prune that
    <*> Gen.prune those
    <*> Gen.prune these
  where
    this  = Gen.word Range.constantBounded
    that  = Gen.text (Range.linear 0 255) Gen.unicode
    those = Gen.unicode
    these = Gen.list (Range.linear 0 100) (Gen.int Range.constantBounded)
