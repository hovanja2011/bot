{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString as B
import Network.HTTP.Conduit
import Network.URL
import Data.Aeson
import Data.Maybe
import Control.Monad

baseUrl :: URL
baseUrl = fromJust $ importURL "https://api.telegram.org"

type Token = String

addPath :: String -> URL -> URL
addPath s url =  url {url_path = oldpath ++ s} where oldpath = url_path url

withToken :: URL -> Token -> URL
withToken url t = url {url_path = oldpath ++ "bot" ++ t } where oldpath = url_path url

getMeUrl :: Token -> URL
getMeUrl t = addPath "/getMe" $ withToken baseUrl t

makeRequest :: FromJSON a => URL -> IO (Maybe a)
makeRequest = liftM decode. simpleHttp . exportURL

getMe :: Token -> IO (Maybe (TelegramResponse User))
getMe = makeRequest . getMeUrl

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

data Update = Update
    { updateId :: Int
    , updateMessage :: Maybe Message
    } deriving (Show)

instance FromJSON Update where
    parseJSON (Object v) = Update <$>
                           v .: "update_id" <*>
                           v .:? "message"
    parseJSON _          = mzero

data Message = Message
    { messageId :: Int
    , messageFrom :: User
    , messageDate :: Int
    } deriving Show

instance FromJSON Message where
    parseJSON (Object v) = Message <$>
                           v .: "message_id" <*>
                           v .: "from" <*>
                           v .: "date" 
    parseJSON _          = mzero    

getUpdatesUrl :: Token -> URL
getUpdatesUrl t = addPath "/getUpdates" $ withToken baseUrl t

getUpdates :: Token -> IO (Maybe (TelegramResponse [Update]))
getUpdates = makeRequest . getUpdatesUrl

main :: IO ()
main = do
  res <- getUpdates "1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA" 
  print (res)

--https://api.telegram.org/bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA/getMe
--bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA
