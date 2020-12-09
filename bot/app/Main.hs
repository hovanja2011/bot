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
import Data.Text as T
import GHC.Generics
-- everything for making request
--begin
token :: B8.ByteString
token = "bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA"

method :: B8.ByteString
method = "/getMe"

makePath :: B8.ByteString -> B8.ByteString -> B8.ByteString
makePath = B8.append

tHost :: B8.ByteString
tHost = "api.telegram.org"

tPath :: B8.ByteString
tPath = makePath token method

buildRequest :: B8.ByteString -> B8.ByteString -> Request
buildRequest host path = setRequestHost host
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest

request :: Request
request = buildRequest tHost tPath
--end

data User = User {
    user_id         :: Int ,
    user_is_bot     :: Bool ,
    user_first_name :: String } deriving (Show)

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
      result <- o.: "result"
      user_id <- result .: "id"
      user_is_bot <- result .: "is_bot"
      user_first_name <- result .: "first_name"
      return (User user_id user_is_bot user_first_name )

getUser :: Request -> IO (User)
getUser request = do 
  t <- httpJSON request
  return (getResponseBody t)

main :: IO ()
main = do
    user <- getUser request
    print (user )
--https://api.telegram.org/bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA/getMe

