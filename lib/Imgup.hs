{-# LANGUAGE OverloadedStrings #-}

module Imgup (uploadAndReturnUrl) where

import qualified Configuration.Dotenv as Dotenv
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy)
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs, getEnv)
import System.FilePath (joinPath, takeExtension, takeFileName)
import System.FilePath.Glob (compile, globDir1)

clientId :: IO BS.ByteString
clientId = do
  Dotenv.loadFile True =<< fromHome ".imgup"
  cid <- getEnv "CLIENT_ID"
  return $ encodeUtf8 $ T.pack cid

fromHome :: String -> IO FilePath
fromHome p = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, p])

getRecentPath :: IO FilePath
getRecentPath = do
  d <- fromHome "Desktop"
  files <- globDir1 (compile "Screen Shot*") d
  case headMaybe (sortBy (flip compare) files) of
    Nothing -> error "no recent screenshots"
    Just a -> return a

fromUrl :: String -> IO String
fromUrl u  = do
  let fileName = "/tmp/" ++ takeFileName u
  r <- get u
  LBS.writeFile fileName (r ^. responseBody)
  pure fileName

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    ["-u", u] -> fromUrl u
    ["--screenshot"]   -> getRecentPath
    (path:_)           -> do
        d <- getCurrentDirectory
        return $ joinPath [d, path]
    _                  -> error "invalid args"

uploadAndReturnUrl :: IO String
uploadAndReturnUrl = do
  imagePath <- parseArgs
  cid <- clientId
  let authHeader = defaults & header "Authorization" .~ ["Client-ID" <> " " <> cid]
  let opts = [ partText "type" "file"
             , partFile "image" imagePath
             ]

  r <- postWith authHeader "https://api.imgur.com/3/image.json" opts
  let guid = r ^. responseBody . key "data" . key "id" . _String
  return $ "http://i.imgur.com/" ++ T.unpack guid ++ takeExtension imagePath

headMaybe :: [a] -> Maybe a
headMaybe (x:_) = Just x
headMaybe []  = Nothing
