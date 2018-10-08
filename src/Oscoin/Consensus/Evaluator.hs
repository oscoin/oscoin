module Oscoin.Consensus.Evaluator
    ( Evaluator
    , EvalError(..)
    , EvalResult

    , identityEval
    , foldEval
    ) where

import           Oscoin.Prelude

import           Codec.Serialise
import           Data.Aeson (FromJSON(..), ToJSON(..))

newtype EvalError = EvalError { fromEvalError :: Text }
    deriving (Eq, Show, Read, Semigroup, Monoid, IsString, Generic)

instance Serialise EvalError

instance ToJSON EvalError where
    toJSON (EvalError e) = toJSON e

instance FromJSON EvalError where
    parseJSON v = EvalError <$> parseJSON v

type EvalResult state output = Either EvalError (output, state)

type Evaluator state tx output = tx -> state -> EvalResult state output

-- | The identity evaluator. An evaluator that accepts any expression and has
-- no state.
identityEval :: Evaluator s a ()
identityEval _ s = Right ((), s)

foldEval :: Monoid w => Evaluator w w ()
foldEval x xs = Right ((), xs <> x)
