{-# LANGUAGE OverloadedStrings #-}

module Interface 
  ( runApp
  , app
  , fizzbuzz_json
  ) where


import           FizzBuzz
import           FizzUtils
import           Data.Aeson 
import           FizzTypes
import           Network.HTTP.Types.Status 
import           Network.Wai (Application)
import qualified Web.Scotty as S
import           Web.Scotty ( ScottyM
                            , scottyApp
                            , scotty
                            , post
                            , jsonData
                            , json
                            , ActionM
                            , rescue
                            , next)
import           Data.Text
import           Data.Either.Combinators (mapBoth)
app' :: S.ScottyM ()
app' = do
  S.post "/fizzbuzz" (fizzbuzz_json) 
  S.post "/fizzbuzz" oopsie


app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'

--fizzbuzz_json :: S.ActionM () 
fizzbuzz_json = do
  mapBoth id fizzBuzzFib <$> configurating >>= S.json
  where
    configurating = (configurator <$> jsonArg `rescue` oopsie')
    jsonArg       = S.jsonData :: ActionM Args
    oopsie'       = const next

oopsie = do
  S.json $ object [ "error" .= ("Invalid request" :: String) ]
  S.status badRequest400
