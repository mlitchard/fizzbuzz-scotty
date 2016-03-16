{-# LANGUAGE MonadComprehensions #-}
module FizzUtils
    ( ffw
    , bf
    , mod3
    , mod5
    , fizzPrime
    , configurator
    , plusPrime
    ) where

import              FizzTypes
--import              FizzBuzz

import              Data.Either.Utils (maybeToEither)
import              Data.List (lookup)
import              Data.Semigroup ((<>))
import              Data.Maybe (fromMaybe)
import              Data.Numbers.Primes (isPrime)
import              Control.Applicative

fizzies = [ (MOD3,mod3)
          , (MOD5,mod5)
          , (PRIME,fizzPrime)
          , (STANDARD, standard)
          , (PLUSPRIME, plusPrime)
          ] 

fibbies :: [(FibbComp,Fibonator)]
fibbies = [(LINEAR,ffw),(CONSTANT,bf)] 

ffw :: Fibonator 
ffw = fib_ffw

bf :: Fibonator 
bf = fib_bf



fib_ffw n = 
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

fib_bf n =
  round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

fibPlus (a, b) (c, d) = (bd - (b - a)*(d - c), a*c + bd)
  where bd = b*d

unFib (a, b) n
  | n < a = (0, 0, 1)
  | n < e = (2*k, c, d)
  | otherwise = (2*k + 1, e, f)
      where
        (k, c, d) = unFib (fibPlus (a, b) (a, b)) n
        (e, f)    = fibPlus (a, b) (c, d)

isFib n = n == a where (k, a, b) = unFib (1, 1) n

--mod3 :: ((Integral a) => (a -> Maybe String)) 
mod3,mod5,fizzPrime :: Fizzanator 
mod3 i = [ "fizz" | i `rem` 3 == 0 ]
               
mod5 i = [ "buzz" | i `rem` 5 == 0 ]
fizzPrime i  = ["boogie down " | isPrime i     ]
standard     = mod3 <> mod5
plusPrime    = mod3 <> mod5 <> fizzPrime


configurator :: Args -> Either FizzError Config
configurator args = 
  Config <$> findFib <*> findFizz <*> upperBound
  where
    findFizz   = maybeToEither NoFibImp (lookup fizzconf fizzies)
    findFib    = maybeToEither NoFizzImp (lookup fibconf fibbies) 
    fizzconf   = fizz args
    fibconf    = fib  args
    upperBound = Right (ub args)
 
--testfib fcn n =
--  let (Right fib_res) = fibb fcn n
--  in isFib fib_res
      
