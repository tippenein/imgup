{-# LANGUAGE OverloadedStrings #-}

module Imgup where

import qualified Configuration.Dotenv as Dotenv
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import Data.List (sortBy)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs, getEnv)
import System.FilePath (joinPath)
import System.FilePath.Glob (compile, globDir1)

imgupConfigPath = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, ".imgup"])

clientId :: IO BS.ByteString
clientId = do
  Dotenv.loadFile True =<< imgupConfigPath
  cid <- getEnv "CLIENT_ID"
  return $ encodeUtf8 $ T.pack cid

headMaybe :: [a] -> Maybe a
headMaybe (x:_) = Just x
headMaybe []  = Nothing

imagePath path = do
  d <- getCurrentDirectory
  return $ joinPath [d, path]

desktopPath = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, "Desktop"])

getRecentPath = do
  d <- desktopPath
  files <- globDir1 (compile "Screen Shot*") d
  case headMaybe (sortBy (flip compare) files) of
    Nothing -> error "no recent screenshots"
    Just a -> return a

parseArgs = do
  args <- getArgs
  case args of
    ["--screenshot"] -> getRecentPath
    (path:_)           -> return path
    _                  -> error "invalid args"

uploadAndReturnUrl :: IO String
uploadAndReturnUrl = do
  path <- parseArgs
  image <- imagePath path
  cid <- clientId
  let authHeader = defaults & header "Authorization" .~ [BS.intercalate " " ["Client-ID", cid]]
  let opts = [ partText "type" "file"
             , partFile "image" image
             ]

  r <- postWith authHeader "https://api.imgur.com/3/image.json" opts
  let guid = r ^. responseBody . key "data" . key "id" . _String
  return $ "http://imgur.com/" ++ T.unpack guid
