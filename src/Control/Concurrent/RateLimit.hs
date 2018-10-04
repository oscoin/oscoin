{-# LANGUAGE CPP #-}

-- | A simple rate limiter as a formulation of the \"token bucket\" algorithm.
--
-- A 'RateLimiter' is constructed with a number of credits by which the bucket
-- is replenished every tick (that is, second), and an initial and maximum
-- balance. A 'rateLimited' action specifies it's cost. If there is sufficient
-- balance, the cost is deducted, and the action is run. Otherwise, nothing is
-- deducted, and the action is not run.
--
-- Usually, the maximum balance is equal to the credits, but it can be higher to
-- support \"burstiness\".
--
-- It is up to the user to interpret what the unit of the bucket's tokens is.
-- For example, to cap some notion of \"request\" to 10 per second, one might
-- use:
--
-- @
--
-- lmtr <- newRateLimiter 10 10
-- rateLimited lmtr 1 $ ...
--
-- @
--
-- In order to limit to a number of bytes per second, one might use:
--
-- @
--
-- lmtr <- newRateLimiter 1024 1024
-- let payload = ...
-- rateLimited lmtr (size payload) $ send payload
--
-- @
module Control.Concurrent.RateLimit
    ( RateLimiter
    , newRateLimiter
    , rateLimited
    )
where

import           Prelude

import           Control.Monad.IO.Class
import           Data.IORef
import qualified System.Clock as Clock

data RateLimiter = RateLimiter
    { credits    :: Double
    , balance    :: IORef Double
    , maxBalance :: Double
    , lastTick   :: IORef Clock.TimeSpec
    }

newRateLimiter
    :: Double -- Credits per second.
    -> Double -- Maximum balance.
    -> IO RateLimiter
newRateLimiter credits maxBalance = do
    balance  <- newIORef maxBalance
    lastTick <- newIORef =<< timeNow
    pure RateLimiter{..}

rateLimited
    :: MonadIO m
    => RateLimiter
    -> Double      -- Cost.
    -> m a         -- Action to rate limit.
    -> m (Maybe a) -- 'Just' the result of the action, or 'Nothing' if the balance was insufficient.
rateLimited RateLimiter{..} cost ma = do
    pass <- liftIO $ do
        now     <- timeNow
        (lst,t) <- atomicModifyIORef' lastTick $ \x -> (now,(x,now))

        atomicModifyIORef' balance $ \bal ->
            let elapsed  = Clock.sec $ Clock.diffTimeSpec lst t
                accrued  = realToFrac elapsed * credits
                balance' = min maxBalance (bal + accrued)
             in if balance' >= cost then
                    (balance' - cost, True)
                else
                    (balance', False)

    if pass then Just <$> ma else pure Nothing

timeNow :: IO Clock.TimeSpec
#if defined(CLOCK_MONOTONIC_COARSE)
timeNow = Clock.getTime Clock.MonotonicCoarse
#else
timeNow = Clock.getTime Clock.Monotonic
#endif
