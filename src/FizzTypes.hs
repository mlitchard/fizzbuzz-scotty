{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module FizzTypes
  ( FizzError  (..)
  , Fibonator   
  , Fizzanator 
  , FizzComp   (..)
  , Args       (..)
  , FibbComp   (..)
  , Config     (..)
  ) where

import           Control.Applicative
import           GHC.Generics

import           Data.Aeson
import           Data.Aeson.TH

data FizzComp 
  = MOD3
  | MOD5
  | PRIME
  | STANDARD 
  | PLUSPRIME
      deriving (Read,Eq,Show,Generic)

$(deriveFromJSON defaultOptions ''FizzComp)
$(deriveToJSON defaultOptions ''FizzComp)

data FibbComp 
  = LINEAR
  | CONSTANT
      deriving (Read,Eq,Show,Generic) 

$(deriveFromJSON defaultOptions ''FibbComp)
$(deriveToJSON defaultOptions ''FibbComp)

type Fibonator  = (Integer -> Integer)
type Fizzanator = (Integer -> Maybe String)

data Config = Config
  { fib_config  :: !Fibonator
  , fiz_config  :: !Fizzanator
  , upper_bound :: !Integer
  } 

data FizzError
  = NoFibImp
  | NoFizzImp
    deriving Eq

instance Show FizzError where
  show NoFibImp     = "You selected a Fib configration " ++ 
                      "that hasn't been implemented"
  show NoFizzImp    = "You selected a Fizz configration " ++
                      "that hasn't been implemented"

data Args = Args
  { fizz :: !FizzComp
  , fib  :: !FibbComp
  , ub   :: !Integer
  } deriving (Show,Generic) 

$(deriveFromJSON defaultOptions ''Args)
$(deriveToJSON defaultOptions ''Args)
$(deriveToJSON defaultOptions ''FizzError)
