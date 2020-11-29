{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple          
import qualified Data.ByteString.Char8         as BS
import Data.Aeson
import Control.Monad
import Control.Applicative

data Token = String

--data Request = BS.ByteString

data TelegramResponse a = TelegramResponse 
    { responseOk :: Bool
    , responseDescription :: Maybe String
    , responseResult :: Maybe a
    } deriving Show

instance FromJSON a => FromJSON (TelegramResponse a) where
    parseJSON (Object v) = TelegramResponse <$>
                           v .: "ok" <*>
                           v .:? "description" <*>
                           v .:? "result"
    parseJSON _          = mzero

fetchJSON :: IO (TelegramResponse a)
fetchJSON = do
  res <- httpJSON "https://api.telegram.org/"
  return (getResponseBody res)


main :: IO (TelegramResponse a)
main = do
  json <- fetchJSON
  BS.putStrLn json
