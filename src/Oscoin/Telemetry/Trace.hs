module Oscoin.Telemetry.Trace
    ( Traced
    , tracing
    , traced
    , Probe(..)
    , hoistProbe
    , probed
    , noProbe

    -- * Running the probes
    , Tracer

    -- * Re-exports from 'Control.Comonad'
    , extract
    ) where

import           Oscoin.Prelude

import           Oscoin.Telemetry.Events
import           Oscoin.Time.Chrono

import           Control.Comonad
import           Control.Monad.State.Strict (modify')


{------------------------------------------------------------------------------
  Tracing events within pure code.
------------------------------------------------------------------------------}

newtype Traced a =
    Traced (StateT (OldestFirst [] NotableEvent) Identity a)
    deriving (Functor, Applicative, Monad)

tracing :: Traced a -> (a, OldestFirst [] NotableEvent)
tracing (Traced t) = runIdentity . flip runStateT mempty $ t

-- | Like 'tracing', but ignores the traced events.
tracing_ :: Traced a -> a
tracing_ (Traced t) = fst . runIdentity . flip runStateT mempty $ t

traced :: NotableEvent -> a -> Traced a
traced evt a = Traced $ do
    modify' $ \nf -> nf `seq` nf <> OldestFirst [evt]
    pure a

instance Comonad Traced where
  extract = tracing_
  -- (Traced a -> b) -> Traced a -> Traced b
  extend fn a = Traced $ do
      let (_, evts) = tracing a
      put $! evts
      pure (fn a)

{------------------------------------------------------------------------------
  Probes to trace events
------------------------------------------------------------------------------}

-- | A 'Probe' over @m@.
data Probe m where
    Probe :: (HasCallStack => NotableEvent -> m ()) -> Probe m

-- | A 'Tracer' is simply a Rank-2 function that also carry a 'CallStack'.
-- This is purely syntactic sugar for library users, as it's tiring and
-- error prone to annotate each \"tracer\" with a 'HasCallStack' constraint.
type Tracer m = HasCallStack => forall a. Traced a -> m a

-- | When given a 'Probe' and a 'Traced' @a@, it collects the traces, output
-- them using the 'Probe' and returns the traced value @a@.
probed :: (HasCallStack, Monad m) => Probe m -> Traced a -> m a
probed (Probe runProbe) t = do
    let (a, evts) = tracing t
    forM_ (toOldestFirst evts) runProbe
    pure a

-- | The \"identity\" probe.
noProbe :: Monad m => Probe m
noProbe = Probe (\_ -> pure ())

-- | Given a natural transformation, transform a 'Probe' operating in a
-- 'Monad' @m@ into a 'Probe' on @n@.
hoistProbe :: (forall x. m x -> n x) -> Probe m -> Probe n
hoistProbe natTrans (Probe fn) = Probe (natTrans . fn)
