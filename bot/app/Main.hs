{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Data.Aeson
import Data.Maybe
import Data.List 
--import Data.Text as T
import GHC.Generics


-- everything for making getMe request
token :: B8.ByteString 
token = "bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA"

method_getMe :: B8.ByteString
method_getMe = "/getMe"

makePath :: B8.ByteString -> B8.ByteString -> B8.ByteString
makePath = B8.append

tHost :: B8.ByteString
tHost = "api.telegram.org"

tPath_getMe :: B8.ByteString
tPath_getMe = makePath token method_getMe

buildRequest :: B8.ByteString -> B8.ByteString -> Request
buildRequest host path = setRequestHost host
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest

request_getMe :: Request
request_getMe = buildRequest tHost tPath_getMe
--end


--building User
data User = User {
    user_id         :: Int ,
    user_is_bot     :: Bool ,
    user_first_name :: String } deriving (Show)
instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
      user_ok <- o.: "ok"
      result <- last <$> o.: "result"
      message <- result .: "message"
      from <- message .: "from"
      user_id <- from .: "id"
      user_is_bot <- from .: "is_bot"
      user_first_name <- from .: "first_name"
      return (User user_id user_is_bot user_first_name )


newtype ResultList = ResultList {resultList :: [Result]}
instance FromJSON ResultList where
    parseJSON (Object o) = ResultList <$> o.: "result"
    parseJSON _ = mzero


data Result = Result {
    result_id         :: Int ,
    result_is_bot     :: Bool ,
    result_first_name :: String } deriving (Show)
instance FromJSON Result where
    parseJSON = withObject "result" $ \o -> do
      message <- o .: "message"
      from <- message .: "from"
      user_id <- from .: "id"
      user_is_bot <- from .: "is_bot"
      user_first_name <- from .: "first_name"
      return (User user_id user_is_bot user_first_name )

getUser :: Request -> IO (User)
getUser request = do 
  t <- httpJSON request
  return (getResponseBody t)
-- end


--here I send message to User
message_help :: B8.ByteString
message_help = "Description about this bot"

method_sendHelp :: Int -> B8.ByteString
method_sendHelp id = makePath (makePath "/sendMessage?chat_id=" (B8.pack.show $ id)) (makePath "&text=" message_help)

tPath_sendHelp :: Int -> B8.ByteString
tPath_sendHelp id = makePath token (method_sendHelp id)

request_sendHelp :: Int -> Request
request_sendHelp id = buildRequest tHost (tPath_sendHelp id)

get_message_response :: Int-> IO (B8.ByteString)
get_message_response id = do
  t <- httpBS (request_sendHelp id)
  return (getResponseBody t)
--Problem. I send message from bot to bot!
--end


-- make getUpdate request
method_getUpdate :: B8.ByteString
method_getUpdate = "/getUpdate"

tPath_getUpdate :: B8.ByteString
tPath_getUpdate = makePath token method_getUpdate

request_getUpdate :: Request
request_getUpdate = buildRequest tHost tPath_getUpdate
-- want to update/universied making request methods 
--end


main :: IO ()
main = do
    user <- getUser request_getMe :: IO (User)
    iid <- return (is_bot user)
   -- body <- get_message_response id
    print (iid )
--https://api.telegram.org/bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA/getMe

