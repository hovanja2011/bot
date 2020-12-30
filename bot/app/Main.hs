{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Network.URL
import Data.Aeson
import Data.Maybe
import Control.Monad
import Data.ByteString.Lazy.UTF8 (toString)

type Token = String

baseURL :: URL
baseURL = fromJust.importURL $ "https://api.telegram.org"

addPath :: String -> URL -> URL
addPath path url = url {url_path = oldPath++path} where oldPath = url_path url

addToken :: Token -> URL -> URL
addToken token url = addPath token $ addPath "bot" baseURL   

makeRequest :: FromJSON a => URL -> IO (Maybe a)
makeRequest = liftM decode.simpleHttp.exportURL 

--Updates block
getUpdatesURL :: Token -> URL
getUpdatesURL token = addPath "/getUpdates" $ addToken token baseURL

getUpdates :: Token -> IO (Maybe (TelegramResponse [Update]))
getUpdates token = makeRequest.getUpdatesURL $ token 
--

data TelegramResponse a = TelegramResponse {
  responseStatus :: Bool,
  responseResult :: Maybe a
} deriving Show

instance FromJSON a => FromJSON (TelegramResponse a) where
  parseJSON (Object o) = TelegramResponse <$>
                         o .: "ok" <*>
                         o .:? "result"
  parseJSON _          = mzero

data Update = Update {
  updateId      :: Int,
  updateMessage :: Maybe Message
} deriving Show

instance FromJSON Update where
  parseJSON (Object o) = Update <$>
                         o.: "update_id" <*>
                         o.:? "message"
  parseJSON _          = mzero

data Message = Message {
  messageId   :: Int,
  messageFrom :: Maybe User,
  messageText :: Maybe String
  } deriving Show

instance FromJSON Message where
  parseJSON (Object o) = Message <$>
                         o.: "message_id" <*>
                         o.:? "from" <*>
                         o.:? "text"
  parseJSON _          = mzero

data User = User {
  userId :: Int,
  userIsBot :: Bool,
  userFirstName :: String
} deriving Show

instance FromJSON User where
  parseJSON (Object o) = User <$>
                         o.: "id" <*>
                         o.: "is_bot" <*>
                         o.: "first_name"
  parseJSON _          = mzero

getMessageFromResponse :: TelegramResponse [Update] -> String
getMessageFromResponse = fromJust.messageText.fromJust.updateMessage.last.fromJust.responseResult 

main :: IO ()
main = do
  token <- return ("1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA")
  upds <- getUpdates token
  print (getMessageFromResponse.fromJust $ upds)
