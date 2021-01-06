{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Network.URL
import Data.Aeson
import Data.Maybe
import Control.Monad
import Data.ByteString.Lazy.UTF8 (toString)

type Token = String

--Response data
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
--end

--work with URL
domain :: URL
domain = fromJust.importURL $ "https://api.telegram.org"

addPath :: String -> URL -> URL
addPath path url = url {url_path = oldPath++path} where oldPath = url_path url

addToken :: Token -> URL -> URL
addToken token url = addPath ("bot" ++ token)  domain    

makeRequest :: FromJSON a => URL -> IO (Maybe a)
makeRequest = liftM decode.simpleHttp.exportURL 

--  params
addParamMessage :: String -> Int -> URL -> URL
addParamMessage ss usid url = addParams [("chat_id", show usid), ("text",ss) ] url

addParam :: (String, String) -> URL -> URL
addParam keyValue url = add_param url keyValue

addParams :: [(String,String)] -> URL -> URL
addParams [] url = url
addParams (x:xs) url = addParams xs $ addParam x url
--  end params

sendMessageURL :: Token -> Int -> String -> URL
sendMessageURL token usid ss =  addParamMessage ss usid $ addPath "/sendMessage" $ addToken token domain 

sendMessage :: Token -> Int -> String -> IO (Maybe (TelegramResponse Message))
sendMessage token usid ss = makeRequest (sendMessageURL token usid ss )
--end

--Updates block
getUpdatesURL :: Token -> URL
getUpdatesURL token = addPath "/getUpdates" $ addToken token domain 

getUpdates :: Token -> IO (Maybe (TelegramResponse [Update]))
getUpdates token = makeRequest.getUpdatesURL $ token 
--end

getlastMessage :: TelegramResponse [Update] -> String
getlastMessage = fromJust.messageText.fromJust.updateMessage.last.fromJust.responseResult 

getId :: TelegramResponse [Update] -> Int
getId = userId.fromJust.messageFrom.fromJust.updateMessage.last.fromJust.responseResult 

react :: String -> String -> Reaction String
react ss mssg = if mssg==ss then NothingNew else
  case mssg of
  "help"    -> NewMessage "Hi, I'm orso bot"
  "break"   -> Stop
  otherwise -> NewMessage mssg

data Reaction a = NothingNew |Stop |NewMessage a
  deriving Show

run :: Token -> String -> IO ()
run token ss = do
  upd  <- getUpdates token
  mssg <- return $ getlastMessage.fromJust $ upd
  text <- return $ react ss mssg
  usid <- return $ getId.fromJust $ upd 
  case text of
    Stop -> putStrLn "we are finished"
    NothingNew -> run token mssg
    NewMessage s -> do
      kek <- sendMessage token usid s
      run token mssg     

main :: IO ()
main = do
  token <- return ("1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA" )  
  upd   <- getUpdates token
  mssg  <- return $ getlastMessage.fromJust $ upd
  run token mssg
