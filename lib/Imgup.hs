{-# LANGUAGE OverloadedStrings #-}

module Imgup where

import qualified Configuration.Dotenv as Dotenv
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs, getEnv)
import System.FilePath (joinPath)

imgupConfigPath = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, ".imgup"])

clientId = do
  Dotenv.loadFile True =<< imgupConfigPath
  cid <- getEnv "CLIENT_ID"
  return $ encodeUtf8 $ T.pack cid

imagePath path = do
  d <- getCurrentDirectory
  return $ joinPath [d, path]

uploadAndReturnUrl = do
  (path:_) <- getArgs
  image <- imagePath path
  cid <- clientId
  let authHeader = defaults & header "Authorization" .~ [BS.intercalate " " ["Client-ID", cid]]
  let opts = [ partText "type" "file"
             , partFile "image" image
             ]

  r <- postWith authHeader "https://api.imgur.com/3/image.json" opts
  let id = r ^. responseBody . key "data" . key "id" . _String
  print $ "http://imgur.com/" ++ T.unpack id
