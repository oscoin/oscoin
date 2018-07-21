{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.Prelude
    ( module Prelude
    , module Control.Applicative
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Writer.CPS
    , module Control.Monad.Trans.Class
    , module Control.Monad.STM.Class
    , module Control.Concurrent.STM
    , module Control.Monad.Fail
    , module Control.Monad
    , module Data.Semigroup
    , module Data.Traversable
    , module Data.Bitraversable
    , module Data.Bifoldable
    , module Data.Foldable
    , module Data.Functor.Identity
    , module Data.Ord
    , module Data.Text
    , module Data.ByteString
    , module Data.Map
    , module Data.IORef
    , module Data.Sequence
    , module Data.List.NonEmpty
    , module Data.Has
    , module Data.Either
    , module Data.Default
    , module Data.ByteArray
    , module Data.Function
    , module Data.Maybe
    , module Data.Functor
    , module Data.String
    , module Data.Word
    , module Data.Set
    , module GHC.Stack
    , module GHC.Generics
    , module GHC.Exts
    , module Debug.Trace
    , MonadIO
    , LByteString
    , Error(..)
    , Timestamp
    , Id
    , notImplemented
    , io
    , pass
    , read
    , readStr
    , readIO
    , readFile
    , (<&>)
    , map
    , foreach
    , encodeUtf8
    , decodeUtf8
    , tshow
    , (++)
    , concat
    , intercalate
    , intersperse
    , sum
    , product
    , identity
    , equal
    , toSeconds
    , chunksOf
    ) where

import           Prelude hiding ( fail, read, readIO, readFile
                                , (++), concat, sum, product, id, map )
import qualified Prelude
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Semigroup (Semigroup, (<>))
import           Data.IORef (IORef)
import           Data.Traversable (Traversable(..), sequence, traverse)
import           Data.Foldable (for_, traverse_, Foldable, toList, null, foldr, foldl', maximumBy, minimumBy)
import           Data.Bitraversable (Bitraversable, bitraverse)
import           Data.Bifoldable (Bifoldable, bifold, bifoldMap)
import           Data.Ord (comparing)
import           Data.Sequence (Seq)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List (intersperse)
import           Data.Has
import           Data.Either (isRight, isLeft, rights, lefts)
import           Data.Default (def, Default)
import           Data.ByteArray (ByteArrayAccess)
import           Data.Function ((&))
import           Data.Functor (void)
import           Data.Functor.Identity (Identity(..), runIdentity)
import           Data.Maybe (fromJust, isJust, isNothing, mapMaybe, fromMaybe)
import           Data.String (IsString)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Word
import           Control.Applicative (liftA2, (<|>))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Reader (Reader, MonadReader, ReaderT(..), runReaderT, ask, asks, local, reader, join)
import           Control.Monad.State (MonadState, runState, runStateT, execStateT, evalStateT)
import           Control.Monad.Writer.CPS (MonadWriter, runWriter, runWriterT, execWriter, tell)
import           Control.Monad.STM.Class (MonadSTM, liftSTM)
import           Control.Monad (forM, mapM, forever)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Monad.Fail (MonadFail, fail)
import           GHC.Stack (HasCallStack)
import           GHC.Generics (Generic)
import           GHC.Exts (IsList(fromList, Item))
import           Debug.Trace (trace, traceShow, traceM, traceShowM, traceShowId)

type LByteString = LBS.ByteString

newtype Error = Error { fromError :: Text }
    deriving (Show, Eq, IsString)

-- | Unix timestamp.
type Timestamp = Word64

-- | 'Id' maps a type to its identifier type. Example: 'Hashed' @tx@ for @tx@.
type family Id a :: *

notImplemented :: HasCallStack => a
notImplemented = error "Not implemented"

-- | > io = liftIO
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Do nothing and return unit inside applicative.
pass :: Applicative f => f ()
pass = pure ()

-- | Parse Text to a value.
read :: Read a => Text -> a
read = Prelude.read . T.unpack

-- | Parse String to a value.
readStr :: Read a => String -> a
readStr = Prelude.read

-- | The readIO function is similar to read
-- except that it signals parse failure to the IO monad
-- instead of terminating the program.
readIO :: Read a => Text -> IO a
readIO = Prelude.readIO . T.unpack

-- | Read a file and return the contents of the file as Text.
-- The entire file is read strictly.
readFile :: FilePath -> IO Text
readFile = T.readFile

-- | Note that this is /not/ the standard @Data.Text.Encoding.decodeUtf8@. That
-- function will throw impure exceptions on any decoding errors. This function
-- instead uses @decodeLenient@.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

-- | Convert a value to readable Text.
tshow :: Show a => a -> Text
tshow = T.pack . Prelude.show

-- | Infix version of foreach.
--
-- @<&>@ is to '<$>' what '&' is to '$'.
infixl 4 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = foreach

-- | > map = fmap
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

-- | > foreach = flip fmap
foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip map

-- | > (++) = mappend
infixr 5 ++
(++) :: Monoid w => w -> w -> w
(++) = mappend

-- | > concat = mconcat
concat :: Monoid w => [w] -> w
concat = mconcat

-- | > intercalate = mconcat .: intersperse
intercalate :: Monoid w => w -> [w] -> w
intercalate xs xss = concat (Data.List.intersperse xs xss)

-- | Compute the sum of a finite list of numbers.
sum :: (Foldable f, Num a) => f a -> a
sum = Data.Foldable.foldl' (+) 0

-- | Compute the product of a finite list of numbers.
product :: (Foldable f, Num a) => f a -> a
product = Data.Foldable.foldl' (*) 1

-- | The identity function.
identity :: a -> a
identity = Prelude.id

-- | Returns True if all values are equal to each other.
equal :: Eq a => [a] -> Bool
equal xs = and $ map (== head xs) (tail xs)

-- | Converts a NominalDiffTime to seconds.
toSeconds :: NominalDiffTime -> Int
toSeconds t = fromEnum t `div` 1000000000000

-- | Splits a list into length-@n@ pieces. If @n@ is @<= 0@, returns an infinite
-- list of empty lists.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0     = take n l : chunksOf n (drop n l)
  | otherwise = repeat []
