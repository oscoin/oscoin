module Test.Hedgehog.Extended
    ( (===)
    , forAll
    , evalEither

    , module H
    ) where

import           Prelude

import           Data.Text (Text, unpack)
import           GHC.Stack

import           Oscoin.Test.Util

import           Hedgehog hiding (evalEither, forAll, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H

-- | Text that is shown without quotes.
newtype Plain = Plain Text

instance Show Plain where
    show (Plain txt) = unpack txt

-- | Drop-in for Hedgehog '===' that relies on the 'Condensed' instance to
-- show the counter example.
(===) :: forall m a. (HasCallStack, MonadTest m, Eq a, Condensed a) => a -> a -> m ()
(===) x y = do
    ok <- withFrozenCallStack $ H.eval (x == y)
    if ok
       then H.success
       else withFrozenCallStack $ H.failDiff (Plain $ condensed x) (Plain $ condensed y)

-- | Drop-in for Hedgehog 'forAll' that relies on the 'Condensed' instance instead
-- of 'Show'.
forAll :: (HasCallStack, Monad m, Condensed a) => Gen a -> PropertyT m a
forAll gen = H.forAllWith (unpack . condensed) gen

-- | Drop-in for Hedgehog 'evalEither' that relies on the 'Condensed' instance instead
-- of 'Show'.
evalEither :: (MonadTest m, Condensed x, HasCallStack) => Either x a -> m a
evalEither = \case
    Left x ->
        withFrozenCallStack $ H.failWith Nothing $ (unpack . condensed) x
    Right x ->
        pure x
