module Oscoin.CLI.Command.Result where

import           Oscoin.Prelude
import           Oscoin.CLI.Radicle

import qualified Radicle as Rad

data Result a =
      ResultOk
    | ResultValue a
    | ResultValues [a]
    | ResultError Text
    deriving (Show, Eq, Functor)

printResult :: Show a => Result a -> IO ()
printResult ResultOk          = putStrLn "<ok>"
printResult (ResultValue a)   = putStrLn (show a)
printResult (ResultValues as) = putStrLn (show as)
printResult (ResultError err) = putStrLn (show err)

instance FromRadicle a => FromRadicle (Result a) where
    fromRadicle (Rad.List xs) = ResultValues $ map fromRadicle xs
    fromRadicle _             = ResultOk
