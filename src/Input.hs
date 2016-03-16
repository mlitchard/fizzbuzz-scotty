module Input
  (mustHaveOne
  ,convertToNatural
  ) where


import              FizzTypes

import              Data.Char (isDigit,toUpper)
import              Text.Read (readMaybe)
import              Data.Either.Utils (maybeToEither)
import              Data.Maybe( fromMaybe )

mustHaveOne :: [String] -> Either FizzError String
mustHaveOne (arg:[]) = Right arg
mustHaveOne []       = Left NoInput
mustHaveOne _        = Left OnlyOne
 

convertToNatural :: String -> Either FizzError Integer
convertToNatural str =
  isNatural =<< toDigit
  where
    toDigit = maybeToEither NotAnInteger (readMaybe str)
    isNatural int = boolToEither (int >= 0) NotNatural int

boolToEither :: Bool -> a -> b -> Either a b
boolToEither bool a b =
  case bool of
    True -> Right b
    False -> Left a



-- one possibility for handling optional file args:
-- -- if no file is provided as argument, read from stdin
-- makeOutput :: Maybe String -> Flag
-- makeOutput ms = Output ( fromMaybe "stdin" ms )
--
-- header = "Usage: main [OPTION...]"
--buildFizzComp :: [String] -> Either FizzError Integer
--buildFizzComp m_comps =
--  map ( readMaybe NotFizzy $ map toUpper) m_comps 
