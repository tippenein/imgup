{-# LANGUAGE OverloadedStrings #-}

module Imgup (uploadAndReturnUrl) where

import qualified Configuration.Dotenv as Dotenv
import Configuration.Dotenv (Config(..))
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs, getEnv)
import System.FilePath (joinPath, takeExtension, takeFileName)
import System.FilePath.Glob (compile, globDir1)

data ImgupConfig
  = ImgupConfig
  { _client_id :: BS.ByteString
  , _pattern :: String
  , _from_directory :: String
  }

getConfig :: IO ImgupConfig
getConfig = do
  p <- fromHome ".imgup"
  _ <- Dotenv.loadFile (Dotenv.Config { configPath = [ p ], configExamplePath = [], configOverride = True})
  cid <- getEnvBS "CLIENT_ID"
  pat <- getEnvDefault "PATTERN" "*.png"
  d <- getEnvDefault "FROM_DIRECTORY" "Desktop"
  pure $ ImgupConfig cid pat d
  where
    getEnvDefault a b = fromMaybe b <$> getEnv' a

    getEnv' a = do
      e <- getEnv a
      let e' = if e == "" then Nothing else Just e
      return e'

    getEnvBS s = do
      e <- getEnv s
      pure $ encodeUtf8 $ T.pack e

fromHome :: String -> IO FilePath
fromHome p = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, p])

getRecentPath :: IO FilePath
getRecentPath = do
  conf <- getConfig
  d <- fromHome $ _from_directory conf
  files <- globDir1 (compile (_pattern conf)) d
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
  conf <- getConfig
  let authHeader = defaults & header "Authorization" .~ ["Client-ID" <> " " <> _client_id conf]
  let opts = [ partText "type" "file"
             , partFile "image" imagePath
             ]

  r <- postWith authHeader "https://api.imgur.com/3/image.json" opts
  let guid = r ^. responseBody . key "data" . key "id" . _String
  return $ "http://i.imgur.com/" ++ T.unpack guid ++ takeExtension imagePath

headMaybe :: [a] -> Maybe a
headMaybe (x:_) = Just x
headMaybe []  = Nothing
