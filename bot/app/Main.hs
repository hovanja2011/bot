{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple          
import qualified Data.ByteString.Char8         as BS
import           Data.Aeson
import           Data.Maybe
import           Control.Monad
import           Control.Applicative
import           Network.URL

data User = User
    { userId :: Int
    , userFirstName :: String
    , userLastName :: Maybe String
    , userUsername :: Maybe String
    } deriving Show

instance FromJSON User where
    parseJSON (Object v) = User <$>
                           v .: "id" <*>
                           v .: "first_name" <*>
                           v .:? "last_name" <*>
                           v .:? "username"
    parseJSON _          = mzero

data Token = T String

data TelegramResponse a = TelegramResponse
    { responseOk :: Bool
    , responseDescription :: Maybe String
    , responseResult :: Maybe a
    } deriving Show

baseURL :: URL
baseURL = fromJust $ importURL "https://api.telegram.org"

getMeUrl :: Token -> URL -> URL
getMeUrl (T token) url= url { url_path = url_path' ++ "/bot" ++ token ++ "/getMe"}
    where url_path' = url_path url

getMe:: Token -> IO (TelegramResponse User)
getMe (T token) =fromJust $ makeRequest ( getMeUrl token baseURL)

makeRequest :: FromJSON a => URL -> IO (Maybe a)
makeRequest = liftM decode . simpleHttp . exportURL

main :: IO (TelegramResponse User)
main = do
  putStrLn "give me your token"
  token <- getLine
  putStrLn $ getMe token
