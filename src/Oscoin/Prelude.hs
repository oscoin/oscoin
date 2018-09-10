{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.Prelude
    ( module Prelude
    , module Control.Applicative
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Writer.CPS
    , module Control.Monad.Trans.Class
    , module Control.Concurrent.STM
    , module Control.Monad.Fail
    , module Control.Monad
    , module Data.Semigroup
    , module Data.Traversable
    , module Data.Bitraversable
    , module Data.Bifoldable
    , module Data.Bifunctor
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
    , Error(..)
    , Timestamp
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
    , rightToMaybe
    , fromRight
    ) where

import           Control.Applicative (liftA2, (<|>))
import           Control.Concurrent.STM (STM, atomically)
import           Control.Monad (forM, forM_, forever, mapM, unless)
import           Control.Monad.Fail (MonadFail, fail)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
                 ( MonadReader
                 , Reader
                 , ReaderT(..)
                 , ask
                 , asks
                 , join
                 , local
                 , reader
                 , runReaderT
                 )
import           Control.Monad.State
                 (MonadState, evalStateT, execStateT, runState, runStateT)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Writer.CPS
                 (MonadWriter, execWriter, runWriter, runWriterT, tell)
import           Data.Bifoldable (Bifoldable, bifold, bifoldMap)
import           Data.Bifunctor (first, second)
import           Data.Bitraversable (Bitraversable, bitraverse)
import           Data.ByteArray (ByteArrayAccess)
import           Data.ByteString (ByteString)
import           Data.Default (Default, def)
import           Data.Either (isLeft, isRight, lefts, rights)
import           Data.Foldable
                 ( Foldable
                 , foldl'
                 , foldr
                 , for_
                 , maximumBy
                 , minimumBy
                 , null
                 , toList
                 , traverse_
                 )
import           Data.Function ((&))
import           Data.Functor (void, ($>))
import           Data.Functor.Identity (Identity(..), runIdentity)
import           Data.Has
import           Data.IORef (IORef)
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import           Data.Maybe
                 (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Sequence (Seq)
import           Data.Set (Set)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as T
import           Data.Time.Clock (NominalDiffTime)
import           Data.Traversable (Traversable(..), for, sequence, traverse)
import           Data.Word
import           Debug.Trace (trace, traceM, traceShow, traceShowId, traceShowM)
import           GHC.Exts (IsList(Item, fromList))
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Prelude hiding
                 ( concat
                 , fail
                 , id
                 , map
                 , product
                 , read
                 , readFile
                 , readIO
                 , sum
                 , (++)
                 )
import qualified Prelude

newtype Error = Error { fromError :: Text }
    deriving (Show, Eq, IsString)

-- | Unix timestamp.
type Timestamp = Word64

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

-- | Maybe get the 'Right' side of an 'Either'.
--
-- @
-- 'rightToMaybe' ≡ 'either' ('const' 'Nothing') 'Just'
-- @
--
-- >>> rightToMaybe (Left 12)
-- Nothing
--
-- >>> rightToMaybe (Right 12)
-- Just 12
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- | Like 'fromJust', for 'Either'.
fromRight :: HasCallStack => Either a b -> b
fromRight (Right x) = x
fromRight _         = error "Either.fromRight: Left"
