{-# LANGUAGE OverloadedStrings #-}

module Imgup where

import qualified Configuration.Dotenv as Dotenv
import Data.Text as T
import Network.Wreq
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs, getEnv)
import System.FilePath (joinPath)

imgupConfigPath = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, ".imgup"])

-- data ImgupConfig
--   = ImgupConfig
--   { client_id :: String
--   }

-- fromDotenv :: [(String, String)] -> Maybe ImgupConfig
-- fromDotenv confVars = ImgupConfig <$> snd cid
--   where cid = select (\a -> "CLIENT_ID" == (fst a)) confVars

clientId = do
  Dotenv.loadFile False <$> imgupConfigPath
  cid <- getEnv "CLIENT_ID"
  return $ T.pack cid

go = do
  image <- getArgs
  post "https://api.imgur.com/3/image.json"
            [ partText "client_id" <$> clientId
            , partText "type" "file"
            , partFile "image" image
            ]
