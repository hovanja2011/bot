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


-- everything for making request (getMe and getUpdate)
data Method = GetMe | GetUpdate
newtype Path = Path B8.ByteString

token :: B8.ByteString 
token = "bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA"

path:: Method -> Path
path m = case m of
    GetMe -> Path (B8.append token "/getMe")
    GetUpdate -> Path (B8.append token "/getUpdate")

host :: B8.ByteString
host = "api.telegram.org"

request ::  Method -> Request
request m = buildRequest host p where Path p = path m

buildRequest :: B8.ByteString -> B8.ByteString -> Request
buildRequest host' path = setRequestHost host'
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest
--end
data User = User {
  id_update :: Int
} deriving Show

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    res <- o.: "result"
    id_update <- res.: "update_id"
    return (User id_update)
--building User


data User = User {
    user_status         :: Bool,
    user_result         :: [Pesult]
    } deriving (Show)

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    user_status <- o.: "ok"
    user_result <- o.: "result"
    return (User user_status user_result) 

data Pesult = Pesult {
    id_update :: Int,
    message   :: Message
} deriving Show

instance FromJSON Pesult where
  parseJSON = withObject "result" $ \o -> do
    id_update <- o.: "update_id"
    message <- o.: "message"
    return (Pesult id_update message) 

data Message = Message {
    id_message  :: Int,
    from        :: From
} deriving Show

instance FromJSON Message where
    parseJSON = withObject "message" $ \o -> do
      id_message <- o.: "message_id"
      from <- o.: "from"
      return (Message id_message from)

data From = From {
    user_id  :: Int
} deriving Show

instance FromJSON From where
    parseJSON = withObject "from" $ \o -> do
      user_id <- o.: "id"
      return (From user_id)  

getUser :: Request -> IO (User)
getUser request = do 
  t <- httpJSON request
  return (getResponseBody t)
-- end


--here I send message to User
message_help :: B8.ByteString
message_help = "Description about this bot"

method_sendHelp :: Int -> B8.ByteString
method_sendHelp id = B8.append (B8.append "/sendMessage?chat_id=" (B8.pack.show $ id)) (B8.append "&text=" message_help)

tPath_sendHelp :: Int -> B8.ByteString
tPath_sendHelp id = B8.append token (method_sendHelp id)

request_sendHelp :: Int -> Request
request_sendHelp id = buildRequest host (tPath_sendHelp id)

get_message_response :: Int-> IO (B8.ByteString)
get_message_response id = do
  t <- httpBS (request_sendHelp id)
  return (getResponseBody t)
--Problem. I send message from bot to bot!
--end

main :: IO ()
main = do
  res <- httpBS (request GetUpdate)
  print (getResponseBody res)
--  user <- fromJust (decode body :: Maybe User)
--  print (user)
     -- body <- get_message_response id

--https://api.telegram.org/bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA/getMe

