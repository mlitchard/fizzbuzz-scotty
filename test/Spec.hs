{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where
-- |
-- Module:      test/Hspec.hs
-- Copyright:   2016 Michael Litchard
-- License:     BSD3
-- Maintainer:  <Michael Litchard> <michael@schmong.org>
-- 
-- A few tests for fizzbuzz demo
--
-- Included are a few unit tests, and one quickcheck for the linear time 
-- fib function. Not: I'll need to investigate HSpec and QuickCheck 
-- to implement sound quickcheck for the constant time fib function

import           Test.Hspec  (Spec,hspec,describe,it,shouldBe)
import           Test.Hspec.Wai  (with,request,shouldRespondWith)
import           Test.Hspec.Wai.JSON  (json)
import           Test.Hspec.QuickCheck (prop)
import           Data.Numbers.Primes (isPrime)
import           Data.ByteString.Lazy (ByteString)

import           Interface (app)
import           FizzBuzz (fib_lin,fib_con)
import           FizzTypes (Fizzanator)

main :: IO ()
main = do
  hspec interface 
  hspec fzzbzz
  hspec qcheck

interface :: Spec
interface = with app $ do
  let header = [("Content-Type","application/json")]
  describe "JSON" $ do
    it " responds with 400" $ do
      request "POST" "/fizzbuzz" header garbage `shouldRespondWith` 400
    it " responds with 200" $ do
      request "POST" "/fizzbuzz" header ex_data `shouldRespondWith` 200

garbage :: ByteString
garbage = [json|[23, {foo: 42}]|]

ex_data :: ByteString
ex_data = [json|{"fizz":"PLUSPRIME","fib":"CONSTANT","ub":10}|]

fzzbzz :: Spec
fzzbzz = do
  describe "Fibonacci check - linear" $
    it "returns a list of integers in linear time" $
      map fib_lin [1 .. 10] `shouldBe` fibs

  describe "Fibonacci check - constant" $
    it "returns a list of integers in constant time" $
      map fib_con [1 .. 10] `shouldBe` fibs

qcheck :: Spec
qcheck = 
  describe "QuickCheck test fib - linear time function" $ do
  prop "QuickCheck test fib_lin" $ testfib fib_lin

modfiz :: Fizzanator -> Integer -> Bool
modfiz fizzbuzz int
    | int <= 0               = True
    | int == 3               = test3
    | int == 5               = test5
    | int `mod` 15 == 0      = testMod35
    | int `mod` 3 == 0       = testMod3
    | int `mod` 5 == 0       = testMod5
    | isPrime int == True    = testPrime
    | otherwise              = testRest
        where
          test3     = Just "fizz boogie down " == fizzbuzz 3
          test5     = Just "buzz boogie down " == fizzbuzz 5
          testMod3  = Just "fizz "             == fizzbuzz int
          testMod5  = Just "buzz "             == fizzbuzz int
          testMod35 = Just "fizz buzz "        == fizzbuzz int
          testPrime = Just "boogie down "      == fizzbuzz int
          testRest  = Just (show int)          == fizzbuzz int

fibPlus (a, b) (c, d) = (bd - (b - a)*(d - c), a*c + bd)
  where 
    bd = b*d

unFib (a, b) n
    | n < a = (0, 0, 1)
    | n < e = (2*k, c, d)
    | otherwise = (2*k + 1, e, f)
        where
          (k, c, d) = unFib (fibPlus (a, b) (a, b)) n
          (e, f)    = fibPlus (a, b) (c, d)

isFib n = n == a 
  where (k, a, b) = unFib (1, 1) n

testfib f n = isFib (f n)

fibs = [1,1,2,3,5,8,13,21,34,55]

fizzBuzzFibs = Right
  [ "1"
  , "1"
  , "boogie down "
  , "fizz boogie down "
  , "buzz boogie down "
  , "8"
  , "boogie down "
  , "fizz "
  , "34"
  , "buzz "
  ]
