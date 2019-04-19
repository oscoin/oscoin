-- | Simple binary decoding utilities.
module Data.Binary.Decoding
    ( GetError
    , Get
    , runGet
    , consumeFromEnd
    , consume
    , getWord8
    ) where

import           Protolude

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data GetError =
      NotEnoughInput ByteString
      deriving (Show, Eq)

-- | A 'Get' is simply an 'ExceptT' transformer which carries around the
-- binary blob being parsed (and consumed), in order to be able to report
-- precise errors in case the parsing process fails.
-- N.B. 'ExceptT' has to be at the top of the stack (as opposed as at the
-- bottom) as this makes easier to compose this 'Get' with more sophisticated
-- ones which might offer domain-specific errors (cfr. 'Oscoin.Crypto.Address.Serialisation).
type Get a = ExceptT GetError (State ByteString) a

runGet :: ByteString -> Get a -> Either GetError a
runGet blob p = evalState (runExceptT p) blob

-- | Consumes some input at the end of the blob. It doesn't consume anything
-- in case of failure.
consumeFromEnd :: Int -> Get ByteString
consumeFromEnd need = do
    x <- BS.unfoldrN need (map swap . BS.unsnoc) <$> lift get
    case x of
      (bs, Just rest) | BS.length bs == need -> do
          lift $ put rest
          pure (BS.reverse bs)
      (bs, _) -> throwError $ NotEnoughInput bs

-- | Consumes some input at the start of the blob. It doesn't consume anything
-- in case of failure.
consume :: Int -> Get ByteString
consume need = do
    x <- BS.unfoldrN need BS.uncons <$> lift get
    case x of
      (bs, Just rest) | BS.length bs == need -> do
          lift $ put rest
          pure bs
      (bs, _) -> throwError $ NotEnoughInput bs

-- | Get a 'Word8', or fails otherwise.
getWord8 :: Get Word8
getWord8 = do
    x <- consume 1
    case BS.unpack x of
      [w] -> pure w
      _   -> throwError $ NotEnoughInput x
