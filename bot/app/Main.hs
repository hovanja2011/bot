{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple            ( httpJSON, httpLBS, httpBS, getResponseBody )
import           Data.Aeson                     
import qualified Data.ByteString.Char8         as BS
import           Data.Text                      ( Text )
import qualified Data.ByteString.Internal as S

data User = User {
    user_id :: Int
  , user_is_bot :: Bool
} deriving (Show)

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "id"
        <*> v .: "is_bot"

fetchJSON :: IO BS.ByteString
fetchJSON = do
  res <- httpLBS "https://api.telegram.org/bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA/getMe"
  return (getResponseBody res)

main :: IO ()
main = do
  json <- fetchJSON
  print ( json)
