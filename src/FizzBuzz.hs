module FizzBuzz
    (
      -- * A Generalized FizzBuzz function
      fizzbuzz

      -- * Linear and constant time fibonacci functions, respectively.
    , fib_lin
    , fib_con

      -- * The driver function 
    , fizzBuzzFib
    ) where

import           Data.Maybe (fromMaybe)

import           FizzTypes (Fizzanator, Config (..))

-- This is a modified version of 
-- http://dave.fayr.am/posts/2012-10-4-finding-fizzbuzz.html

-- Here's the gist
-- the comprehensions are type Maybe String
-- Data.SemiGroup imports 
fizzbuzz :: Fizzanator -> Integer -> String
fizzbuzz f i = fromMaybe (show i) $ f i

-- https://wiki.haskell.org/The_Fibonacci_sequence#Constant-time_implementations
fib_con :: Integer -> Integer
fib_con n = (round $ phi ** fromIntegral n / sq5)
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

-- https://wiki.haskell.org/The_Fibonacci_sequence#Fastest_Fib_in_the_West
fib_lin :: Integer -> Integer
fib_lin n = 
  snd . foldl fib' (1, 0) . map (toEnum . fromIntegral) $ unfoldl divs n
    where
      unfoldl f x = 
        case f x of
          Nothing     -> []
          Just (u, v) -> unfoldl f v ++ [u]
 
      divs 0 = Nothing
      divs k = Just (uncurry (flip (,)) (k `divMod` 2))
 
      fib' (f, g) p
          | p         = (f*(f+2*g), f^2 + g^2)
          | otherwise = (f^2+g^2,   g*(2*f-g))

fizzBuzzFib :: Config -> [String]
fizzBuzzFib config = fizzies $ fibbies
  where
    fizzies    = map (fizzbuzz fizz_func)
    fibbies    = map fib_func [1 .. upper_bound']
    fizz_func  = fiz_config config
    fib_func   = fib_config config
    upper_bound' = upper_bound config
