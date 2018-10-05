module Oscoin.Consensus.Evaluator
    ( Evaluator
    , EvalError(..)
    , EvalResult

    , identityEval
    , foldEval
    ) where

import           Oscoin.Prelude

newtype EvalError = EvalError { fromEvalError :: Text }
    deriving (Eq, Show, Read, Semigroup, Monoid, IsString)

type EvalResult state output = Either [EvalError] (output, state)

type Evaluator state tx output = tx -> state -> EvalResult state output

-- | The identity evaluator. An evaluator that accepts any expression and has
-- no state.
identityEval :: Evaluator s a ()
identityEval _ s = Right ((), s)

foldEval :: Monoid w => Evaluator w w ()
foldEval x xs = Right ((), xs <> x)
