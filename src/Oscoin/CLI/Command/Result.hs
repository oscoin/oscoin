module Oscoin.CLI.Command.Result where

import           Oscoin.Prelude

data Result a =
      ResultOk
    | ResultValue a
    | ResultValues [a]
    | ResultError Text
    deriving (Show, Eq, Functor)

printResult :: Show a => Result a -> IO ()
printResult r = putStrLn $ case r of
    ResultOk        -> "<ok>" :: Text
    ResultValue a   -> show a
    ResultValues as -> show as
    ResultError err -> show err
