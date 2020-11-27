{-# LANGUAGE OverloadedStrings #-}

module Main where




import           Network.HTTP.Simple            ( httpBS, getResponseBody )               
import qualified Data.ByteString.Char8         as BS


fetchJSON :: IO BS.ByteString
fetchJSON = do
  res <- httpBS "https://api.telegram.org/bot1421138697:AAHfmKgs38ODbldkqE3jlGEikQlaNuXsOXA/getUpdates"
  return (getResponseBody res)


main :: IO ()
main = do
  json <- fetchJSON
  BS.putStrLn json