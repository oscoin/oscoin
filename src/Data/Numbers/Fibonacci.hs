-- Inlining of @http://hackage.haskell.org/package/fibonacci@.
-- All credit goes to the original author.

-- |
-- Module      : Data.Numbers.Fibonacci
-- Copyright   : Sebastian Fischer
-- License     : BSD3
--
-- Maintainer  : Sebastian Fischer
-- Stability   : experimental
-- Portability : portable
--
-- Fast computation of Fibonacci numbers. Use version @0.1.*@ if you
-- prefer the Fibonacci sequence to start with one instead of
-- zero. Version @0.2.*@ adds correct handling of negative arguments
-- and changes the implementation to satisfy @fib 0 = 0@.
--
-- See <http://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form> for
-- a description of the employed method.
--
module Data.Numbers.Fibonacci ( fib ) where

import           Prelude

-- |
-- Computes Fibonacci numbers. Yields the same results as
--
-- > fib 0         = 0
-- > fib 1         = 1
-- > fib n | n > 1 = fib (n-2) + fib (n-1)
-- >       | n < 0 = (-1)^(1-n) * fib (-n)
--
-- but more efficiently.
--
-- Examples:
--
-- @
-- ghci> map fib [0..9]
-- [0,1,1,2,3,5,8,13,21,34]
-- ghci> map (fib . negate) [0..9]
-- [0,1,-1,2,-3,5,-8,13,-21,34]
-- ghci> length . show $ fib 10000000
-- 2089877
-- @
--
fib :: (Integral int, Num num) => int -> num
fib n | n >= 0    = upperRight $ matrixPower (Matrix 1 1 0) n
      | even n    = negate . fib $ negate n
      | otherwise = fib $ negate n

{-# SPECIALISE fib :: Int -> Int     #-}
{-# SPECIALISE fib :: Int -> Integer #-}

-- See http://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form

-- Fibonacci numbers can be computed by exponentiation of symmetric
-- 2x2 matrices which we represent as triples. We have for all n:
--
--     (Matrix 1 1 0)^n = Matrix (fib (n+1)) (fib n) (fib (n-1))
--
data Matrix a = Matrix a a a

upperRight :: Matrix a -> a
upperRight (Matrix _ a _) = a

-- We implement exponentiation of matrices by repeated squaring.

matrixPower :: (Integral int, Num num) => Matrix num -> int -> Matrix num
matrixPower _ 0             = Matrix 1 0 1
matrixPower m 1             = m
matrixPower m n | r == 0    = square $ matrixPower m q
                | otherwise = times m . square $ matrixPower m q
 where (q,r) = quotRem n 2

{-# SPECIALISE matrixPower :: Matrix Int     -> Int -> Matrix Int     #-}
{-# SPECIALISE matrixPower :: Matrix Integer -> Int -> Matrix Integer #-}

square :: Num num => Matrix num -> Matrix num
square m = times m m

{-# SPECIALISE square :: Matrix Int     -> Matrix Int     #-}
{-# SPECIALISE square :: Matrix Integer -> Matrix Integer #-}

times :: Num num => Matrix num -> Matrix num -> Matrix num
times (Matrix a b c) (Matrix x y z) = Matrix (a*x + by) (a*y + b*z) (by + c*z)
 where by = b*y

{-# SPECIALISE times :: Matrix Int     -> Matrix Int     -> Matrix Int     #-}
{-# SPECIALISE times :: Matrix Integer -> Matrix Integer -> Matrix Integer #-}
